module type Scalar = Tree_intf.Scalar
module type Element = Tree_intf.Element
module type Quadtree = Tree_intf.Quadtree
module type Box2D = Tree_intf.Box2D
module type Point2D = Tree_intf.Point2D

module MakeRect = Box.Make2D
module MakePoint = Point.Make2D

let ( >> ) f g x = g @@ f x

module Quadtree (Num : Scalar) (E : Element with type n = Num.t) :
  Quadtree with type elt = E.t and type n = Num.t = struct
  module Point = MakePoint (Num)

  module Box : Box2D with type n = Num.t and type point = Point.t =
    MakeRect (Num) (Point)

  type n = Num.t
  type elt = E.t

  let position (elt : E.t) : Point.t =
    let x, y = E.position elt in
    Point.point x y

  type tree =
    | Node of (Box.t * tree list)
    | Leaf of (Box.t * E.t list)
    | Empty of Box.t

  type t = { capacity : int; tree : tree }

  let empty domain capacity = { capacity; tree = Empty domain }

  let load ({ capacity; tree } as t) (es : E.t list) =
    let partition (lu, ru, rd, ld) es' =
      let belongs box' = List.filter (position >> Box.contains box') in
      ( (lu, belongs lu es'),
        (ru, belongs ru es'),
        (rd, belongs rd es'),
        (ld, belongs ld es') )
    in
    let rec load' (box', es') =
      if List.length es' >= capacity then
        let quarters = Box.split box' in
        let lu, ru, rd, ld = partition quarters es' in
        Node (box', List.map load' [ lu; ru; rd; ld ])
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
          let nw, ne, se, sw = Box.split box in
          let es' = elt :: es in
          let leaves =
            List.map
              (fun box' ->
                Leaf (box', List.filter (position >> Box.contains box') es'))
              [ nw; ne; se; sw ]
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
    let distance ({ x; y } : Point.t) ({ x = x'; y = y' } : Point.t) =
      let dx = Num.sub x x' in
      let dy = Num.sub y y' in
      Num.add (Num.mul dx dx) (Num.mul dy dy)
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
                  (distance (Box.midpoint @@ get_box n1) pt)
                  (distance (Box.midpoint @@ get_box n2) pt))
              ns
          in
          map_while search sorted
      | Leaf (_, es) ->
          let sorted =
            List.fast_sort
              (fun e1 e2 ->
                compare (distance (position e1) pt) (distance (position e2) pt))
              es
          in
          map_while
            (fun elt' -> if position elt' <> pt then Some elt' else None)
            sorted
      | Empty _ -> None
    in
    search t.tree
end
