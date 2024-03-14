module type Scalar = Tree_intf.Scalar
module type Element2D = Tree_intf.Element2D
module type Element3D = Tree_intf.Element3D
module type Box2D = Tree_intf.Box2D
module type Point2D = Tree_intf.Point2D
module type Box3D = Tree_intf.Box3D
module type Point3D = Tree_intf.Point3D
module type Quadtree = Tree_intf.Quadtree
module type Octree = Tree_intf.Octree

module MakeRect = Box.Make2D
module MakePoint = Point.Make2D
module MakeCube = Box.Make3D
module MakePoint3D = Point.Make3D

let ( >> ) f g x = g @@ f x

module Quadtree (Num : Scalar) (E : Element2D with type n = Num.t) :
  Quadtree with type elt = E.t and type n = Num.t = struct
  module Point : Point2D with type n = Num.t = MakePoint (Num)

  module Box : Box2D with type n = Num.t and type point = Point.t =
    MakeRect (Num) (Point)

  type n = Num.t
  type elt = E.t

  let position (elt : E.t) : Point.t =
    let x, y = E.position elt in
    Point.point x y

  type tree =
    | Node of (Box.t * tree array)
    | Leaf of (Box.t * E.t array)
    | Empty of Box.t

  type t = { capacity : int; tree : tree }

  let empty domain capacity = { capacity; tree = Empty domain }

  let load ({ capacity; tree } as t) (elements : E.t array) =
    let partition (nw, ne, se, sw) elts =
      let partition' (leaves, elts_seq) box =
        let belong, dont_belong =
          Seq.partition (position >> Box.contains box) elts_seq
        in
        let leaf = (box, Array.of_seq belong) in
        (Iter.cons leaf leaves, dont_belong)
      in
      let leaves, _ =
        Iter.fold partition'
          (Iter.empty, Array.to_seq elts)
          (Iter.of_array [| nw; ne; se; sw |])
      in
      let leaves = Iter.to_list leaves in
      match leaves with
      | [ nw'; ne'; se'; sw' ] -> (nw', ne', se', sw')
      | _ ->
          failwith
            "partition has created a list the wrong # of elts: this should be \
             impossible"
    in
    let rec load' (box', es') =
      if Array.length es' >= capacity then
        let quarters = Box.split box' in
        let nw, ne, se, sw = partition quarters es' in
        Node (box', Array.map load' [| nw; ne; se; sw |])
      else Leaf (box', es')
    in
    match tree with
    | Empty bounds -> { t with tree = load' (bounds, elements) }
    | _ -> failwith "Load can only take an empty tree!"

  let insert ({ capacity; tree } as t) elt =
    let pos = position elt in
    let rec insert' = function
      | Leaf (box, es) when Array.length es < pred capacity ->
          Leaf (box, Array.append es [| elt |])
      | Leaf (box, es) ->
          let nw, ne, se, sw = Box.split box in
          let es' = Seq.init (Array.length es) (fun i -> es.(i)) in
          let leaves =
            Array.map
              (fun box' ->
                Leaf
                  ( box',
                    Array.of_seq
                    @@ Seq.filter (position >> Box.contains box') es' ))
              [| nw; ne; se; sw |]
          in
          Node (box, leaves)
      | Node (box, _) as node' when not @@ Box.contains box pos -> node'
      | Node (box, ns) -> Node (box, Array.map insert' ns)
      | Empty box -> Leaf (box, [| elt |])
    in
    { t with tree = insert' tree }

  let size t =
    let rec aux n = function
      | Node (_, ns) ->
          n + (Array.to_seq ns |> Seq.map (aux 0) |> Seq.fold_left ( + ) 0)
      | Leaf (_, es) -> Array.length es
      | Empty _ -> 0
    in
    aux 0 t.tree

  let remove t elt =
    let pos = position elt in
    let rec remove' = function
      | Node (box, ns) when Box.contains box pos ->
          Node (box, Array.map remove' ns)
      | Leaf (box, es) when Box.contains box pos ->
          Leaf
            ( box,
              Array.to_seq es
              |> Seq.filter (fun elt' -> elt <> elt')
              |> Array.of_seq )
      | t -> t
    in
    { t with tree = remove' t.tree }

  let find (search : elt -> bool) { capacity = _; tree } : elt option =
    let rec find' = function
      | Node (_, ns) -> Iter.of_array ns |> Iter.find_map find'
      | Leaf (_, es) ->
          Iter.of_array es
          |> Iter.find (function elt when search elt -> Some elt | _ -> None)
      | Empty _ -> None
    in
    find' tree

  let map f t =
    let rec map' = function
      | Node (box, ns) ->
          Array.map_inplace map' ns;
          Node (box, ns)
      | Leaf (box, es) ->
          Array.map_inplace f es;
          Leaf (box, es)
      | t -> t
    in
    { t with tree = map' t.tree }

  let iter f t =
    let rec iter' = function
      | Node (_, ns) -> Array.iter iter' ns
      | Leaf (_, es) -> Array.iter f es
      | Empty _ -> ()
    in
    iter' t.tree

  let range range t =
    let rec range' = function
      | Node (box, ns) when Box.intersects box range ->
          Iter.(of_array ns |> map range' |> concat)
      | Leaf (box, es) when Box.intersects box range ->
          let it = Iter.of_array es in
          let filtered = Iter.filter (position >> Box.contains range) it in
          filtered
      | _ -> Iter.empty
    in
    range' t.tree |> Iter.to_list

  let apply f t =
    let rec apply' = function
      | Node (box, ns) -> Node (box, Array.map apply' ns)
      | Leaf (box, es) -> Leaf (box, f es)
      | t -> t
    in
    { t with tree = apply' t.tree }

  let filter f t = apply (Iter.of_array >> Iter.filter f >> Iter.to_array) t

  let filter_map f t =
    apply (Array.to_seq >> Seq.filter_map f >> Array.of_seq) t

  let nearest t pt =
    let get_box = function
      | Node (box, _) -> box
      | Leaf (box, _) -> box
      | Empty box -> box
    in
    let rec search = function
      | Node (_, ns) ->
          Iter.of_array ns
          |> Iter.sort ~cmp:(fun n1 n2 ->
                 compare
                   (Point.distance (Box.midpoint @@ get_box n1) pt)
                   (Point.distance (Box.midpoint @@ get_box n2) pt))
          |> Iter.find_map search
      | Leaf (_, es) ->
          Iter.of_array es
          |> Iter.sort ~cmp:(fun e1 e2 ->
                 compare
                   (Point.distance (position e1) pt)
                   (Point.distance (position e2) pt))
          |> Iter.find (fun elt' ->
                 if position elt' <> pt then Some elt' else None)
      | Empty _ -> None
    in
    search t.tree
end

module Octree (Num : Scalar) (E : Element3D with type n = Num.t) :
  Octree with type elt = E.t and type n = Num.t = struct
  type n = Num.t

  module Point : Point3D with type n = n = MakePoint3D (Num)

  module Box : Box3D with type n = n and type point = Point.t =
    MakeCube (Num) (Point)

  type elt = E.t

  let position elt =
    let x, y, z = E.position elt in
    Point.point x y z

  type tree =
    | Node of Box.t * tree list
    | Leaf of Box.t * elt list
    | Empty of Box.t

  type t = { capacity : int; tree : tree }

  let empty domain capacity = { capacity; tree = Empty domain }

  let load ({ capacity; tree } as t) (es : E.t list) =
    let partition
        ( front_top_left,
          front_top_right,
          front_bottom_left,
          front_bottom_right,
          back_top_left,
          back_top_right,
          back_bottom_left,
          back_bottom_right ) es' =
      let belongs box' = List.filter (position >> Box.contains box') in
      ( (front_top_left, belongs front_top_left es'),
        (front_top_right, belongs front_top_right es'),
        (front_bottom_left, belongs front_bottom_left es'),
        (front_bottom_right, belongs front_bottom_right es'),
        (back_top_left, belongs back_top_left es'),
        (back_top_right, belongs back_top_right es'),
        (back_bottom_left, belongs back_bottom_left es'),
        (back_bottom_right, belongs back_bottom_right es') )
    in
    let rec load' (box', es') =
      if List.length es' >= capacity then
        let quarters = Box.split box' in
        let ( front_top_left,
              front_top_right,
              front_bottom_left,
              front_bottom_right,
              back_top_left,
              back_top_right,
              back_bottom_left,
              back_bottom_right ) =
          partition quarters es'
        in
        Node
          ( box',
            List.map load'
              [
                front_top_left;
                front_top_right;
                front_bottom_left;
                front_bottom_right;
                back_top_left;
                back_top_right;
                back_bottom_left;
                back_bottom_right;
              ] )
      else Leaf (box', es')
    in
    match tree with
    | Empty domain -> { t with tree = load' (domain, es) }
    | _ -> failwith "Load can only take an empty tree!"

  let insert ({ capacity; tree } as t) elt =
    let pos = position elt in
    let rec insert' = function
      | Leaf (box, es) when List.length es < capacity -> Leaf (box, elt :: es)
      | Leaf (box, es) ->
          let ( front_top_left,
                front_top_right,
                front_bottom_left,
                front_bottom_right,
                back_top_left,
                back_top_right,
                back_bottom_left,
                back_bottom_right ) =
            Box.split box
          in
          let es' = elt :: es in
          let leaves =
            List.map
              (fun box' ->
                Leaf (box', List.filter (position >> Box.contains box') es'))
              [
                front_top_left;
                front_top_right;
                front_bottom_left;
                front_bottom_right;
                back_top_left;
                back_top_right;
                back_bottom_left;
                back_bottom_right;
              ]
          in
          Node (box, leaves)
      | Node (box, _) as node' when not @@ Box.contains box pos -> node'
      | Node (box, ns) -> Node (box, List.map insert' ns)
      | Empty box -> Leaf (box, [ elt ])
    in
    { t with tree = insert' tree }

  let size t =
    let rec aux n = function
      | Node (_, ns) -> n + (List.map (aux 0) ns |> List.fold_left ( + ) 0)
      | Leaf (_, es) -> List.length es
      | Empty _ -> 0
    in
    aux 0 t.tree

  let remove t elt =
    let pos = position elt in
    let rec remove' = function
      | Node (box, ns) when Box.contains box pos ->
          Node (box, List.map remove' ns)
      | Leaf (box, es) when Box.contains box pos ->
          Leaf (box, List.filter (fun elt' -> elt <> elt') es)
      | t -> t
    in
    { t with tree = remove' t.tree }

  let find search { capacity = _; tree } =
    let rec find' = function
      | Node (_, ns) ->
          Option.join @@ List.find_opt Option.is_some (List.map find' ns)
      | Leaf (_, es) -> List.find_opt search es
      | Empty _ -> None
    in
    find' tree

  let map f t =
    let rec map' = function
      | Node (box, ns) -> Node (box, List.map map' ns)
      | Leaf (box, es) -> Leaf (box, List.map f es)
      | t -> t
    in
    { t with tree = map' t.tree }

  let iter f t =
    let rec iter' = function
      | Node (_, ns) -> List.iter iter' ns
      | Leaf (_, es) -> List.iter f es
      | Empty _ -> ()
    in
    iter' t.tree

  let range range t =
    let rec range' = function
      | Node (box, ns) when Box.intersects box range ->
          List.concat_map range' ns
      | Leaf (_, es) ->
          List.filter (fun e -> position e |> Box.contains range) es
      | _ -> []
    in
    range' t.tree

  let apply f t =
    let rec apply' = function
      | Node (box, ns) -> Node (box, List.map apply' ns)
      | Leaf (box, es) -> Leaf (box, f es)
      | t -> t
    in
    { t with tree = apply' t.tree }

  let filter f t = apply (List.filter f) t
  let filter_map f t = apply (List.filter_map f) t

  let nearest t pt =
    let get_box = function
      | Node (box, _) -> box
      | Leaf (box, _) -> box
      | Empty box -> box
    in
    let rec map_while f = function
      | x :: xs ->
          let res = f x in
          if Option.is_some res then res else map_while f xs
      | [] -> None
    in
    let rec search = function
      | Node (_, ns) ->
          let sorted =
            List.fast_sort
              (fun n1 n2 ->
                compare
                  (Point.distance (Box.midpoint @@ get_box n1) pt)
                  (Point.distance (Box.midpoint @@ get_box n2) pt))
              ns
          in
          map_while search sorted
      | Leaf (_, es) ->
          let sorted =
            List.fast_sort
              (fun e1 e2 ->
                compare
                  (Point.distance (position e1) pt)
                  (Point.distance (position e2) pt))
              es
          in
          map_while
            (fun elt' -> if position elt' <> pt then Some elt' else None)
            sorted
      | Empty _ -> None
    in
    search t.tree
end
