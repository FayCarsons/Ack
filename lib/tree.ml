include Tree_intf
module MakePoint = Point.Make
module MakeRect = Box.Make
module MakePoint3D = Point.Make3D
module MakeCube = Box.Make3D

let ( >> ) f g x = g @@ f x

module Quadtree (Num : Scalar) (E : Element2D with type n = Num.t) :
  Quadtree with type elt = E.t and type n = Num.t = struct
  module Point : Point with type n = Num.t = MakePoint (Num)

  module Box : Box with type n = Num.t and type point = Point.t =
    Box.Make (Num) (Point)

  type n = Num.t
  type elt = E.t

  let position (elt : E.t) : Point.t =
    let x, y = E.position elt in
    Point.point x y

  type tree =
    | Node of (Box.t * tree array)
    | Leaf of (Box.t * E.t list)
    | Empty of Box.t

  type t = { capacity : int; tree : tree }

  let empty domain capacity = { capacity; tree = Empty domain }

  let load ({ capacity; tree } as t) elements =
    let partition children elts =
      let partition' (leaves, elts_seq) box =
        let belong, dont_belong =
          List.partition (position >> Box.contains box) elts_seq
        in
        let leaf = (box, belong) in
        (leaf :: leaves, dont_belong)
      in
      let leaves, _ = Array.fold_left partition' ([], elts) children in
      Array.of_list leaves
    in
    let rec load' (box', es') =
      if List.length es' >= capacity then
        let quarters = Box.split box' in
        let children' = partition quarters es' |> Array.map load' in
        Node (box', children')
      else Leaf (box', es')
    in
    match tree with
    | Empty bounds -> { t with tree = load' (bounds, elements) }
    | _ -> failwith "Load can only take an empty tree!"

  let insert ({ capacity; tree } as t) elt =
    let pos = position elt in
    let rec insert' = function
      | Leaf (box, es) when List.length es <= pred capacity ->
          Leaf (box, elt :: es)
      | Leaf (box, es) ->
          let children = Box.split box in
          let es' = Iter.of_list es in
          let leaves =
            Array.map
              (fun box' ->
                Leaf
                  ( box',
                    Iter.to_list
                    @@ Iter.filter (position >> Box.contains box') es' ))
              children
          in
          Node (box, leaves)
      | Node (box, _) as node' when not @@ Box.contains box pos -> node'
      | Node (box, ns) -> Node (box, Array.map insert' ns)
      | Empty box -> Leaf (box, [ elt ])
    in
    { t with tree = insert' tree }

  let size t =
    let rec aux n = function
      | Node (_, ns) ->
          n + (Iter.of_array ns |> Iter.map (aux 0) |> Iter.fold ( + ) 0)
      | Leaf (_, es) -> List.length es
      | Empty _ -> 0
    in
    aux 0 t.tree

  let depth t =
    let rec depth' n = function
      | Node (_, ns) -> Array.map (depth' @@ succ n) ns |> Array.fold_left max n
      | Leaf _ -> succ n
      | Empty _ -> n
    in
    depth' 0 t.tree

  let remove t elt =
    let pos = position elt in
    let rec remove' = function
      | Node (box, ns) when Box.contains box pos ->
          Node (box, Array.map remove' ns)
      | Leaf (box, es) when Box.contains box pos ->
          Leaf
            ( box,
              Iter.of_list es
              |> Iter.filter (fun elt' -> elt <> elt')
              |> Iter.to_list )
      | t -> t
    in
    { t with tree = remove' t.tree }

  let find search { capacity = _; tree } =
    let rec find' = function
      | Node (_, ns) -> Array.find_map find' ns
      | Leaf (_, es) -> List.find_opt search es
      | Empty _ -> None
    in
    find' tree

  let map f t =
    let rec map' = function
      | Node (box, ns) ->
          Array.map_inplace map' ns;
          Node (box, ns)
      | Leaf (box, es) -> Leaf (box, List.map f es)
      | t -> t
    in
    { t with tree = map' t.tree }

  let iter f t =
    let rec iter' = function
      | Node (_, ns) -> Array.iter iter' ns
      | Leaf (_, es) -> List.iter f es
      | Empty _ -> ()
    in
    iter' t.tree

  let range range t =
    let rec range' = function
      | Node (box, ns) when Box.intersects box range ->
          Iter.(of_array ns |> map range' |> concat)
      | Leaf (box, es) when Box.intersects box range ->
          Iter.of_list es |> Iter.filter (position >> Box.contains range)
      | _ -> Iter.empty
    in
    range' t.tree |> Iter.to_list

  let apply f t =
    let rec apply' = function
      | Node (box, ns) ->
          Array.map_inplace apply' ns;
          Node (box, ns)
      | Leaf (box, es) -> Leaf (box, f es)
      | t' -> t'
    in
    { t with tree = apply' t.tree }

  let filter f t = apply (Iter.of_list >> Iter.filter f >> Iter.to_list) t
  let filter_map f t = apply (List.to_seq >> Seq.filter_map f >> List.of_seq) t

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
          Iter.of_list es
          |> Iter.sort ~cmp:(fun e1 e2 ->
                 compare
                   (Point.distance (position e1) pt)
                   (Point.distance (position e2) pt))
          |> Iter.find (function
               | elt when position elt <> pt -> Some elt
               | _ -> None)
      | Empty _ -> None
    in
    search t.tree
end

module Octree (Num : Scalar) (E : Element3D with type n = Num.t) :
  Octree with type elt = E.t and type n = Num.t = struct
  type n = Num.t

  module Point : Point3D with type n = Num.t = MakePoint3D (Num)

  module Box : Box with type n = Num.t and type point = Point.t =
    MakeCube (Num) (Point)

  type elt = E.t

  let position elt =
    let x, y, z = E.position elt in
    Point.point x y z

  type tree =
    | Node of Box.t * tree array
    | Leaf of Box.t * elt list
    | Empty of Box.t

  type t = { capacity : int; tree : tree }

  let empty domain capacity = { capacity; tree = Empty domain }

  let load ({ capacity; tree } as t) elements =
    let partition children elts =
      let partition' (leaves, elts_seq) box =
        let belong, dont_belong =
          List.partition (position >> Box.contains box) elts_seq
        in
        let leaf = (box, belong) in
        (leaf :: leaves, dont_belong)
      in
      let leaves, _ = Array.fold_left partition' ([], elts) children in
      Array.of_list leaves
    in
    let rec load' (box', es') =
      if List.length es' >= capacity then
        let quarters = Box.split box' in
        let children' = partition quarters es' |> Array.map load' in
        Node (box', children')
      else Leaf (box', es')
    in
    match tree with
    | Empty bounds -> { t with tree = load' (bounds, elements) }
    | _ -> failwith "Load can only take an empty tree!"

  let insert ({ capacity; tree } as t) elt =
    let pos = position elt in
    let rec insert' = function
      | Leaf (box, es) when List.length es <= pred capacity ->
          Leaf (box, elt :: es)
      | Leaf (box, es) ->
          let children = Box.split box in
          let es' = Iter.of_list es in
          let leaves =
            Array.map
              (fun box' ->
                Leaf
                  ( box',
                    Iter.to_list
                    @@ Iter.filter (position >> Box.contains box') es' ))
              children
          in
          Node (box, leaves)
      | Node (box, _) as node' when not @@ Box.contains box pos -> node'
      | Node (box, ns) -> Node (box, Array.map insert' ns)
      | Empty box -> Leaf (box, [ elt ])
    in
    { t with tree = insert' tree }

  let size t =
    let rec aux n = function
      | Node (_, ns) ->
          n + (Iter.of_array ns |> Iter.map (aux 0) |> Iter.fold ( + ) 0)
      | Leaf (_, es) -> List.length es
      | Empty _ -> 0
    in
    aux 0 t.tree

  let depth t =
    let rec depth' n = function
      | Node (_, ns) -> Array.map (depth' @@ succ n) ns |> Array.fold_left max n
      | Leaf _ -> succ n
      | Empty _ -> n
    in
    depth' 0 t.tree

  let remove t elt =
    let pos = position elt in
    let rec remove' = function
      | Node (box, ns) when Box.contains box pos ->
          Node (box, Array.map remove' ns)
      | Leaf (box, es) when Box.contains box pos ->
          Leaf
            ( box,
              Iter.of_list es
              |> Iter.filter (fun elt' -> elt <> elt')
              |> Iter.to_list )
      | t -> t
    in
    { t with tree = remove' t.tree }

  let find search { capacity = _; tree } =
    let rec find' = function
      | Node (_, ns) -> Array.find_map find' ns
      | Leaf (_, es) -> List.find_opt search es
      | Empty _ -> None
    in
    find' tree

  let map f t =
    let rec map' = function
      | Node (box, ns) ->
          Array.map_inplace map' ns;
          Node (box, ns)
      | Leaf (box, es) -> Leaf (box, List.map f es)
      | t -> t
    in
    { t with tree = map' t.tree }

  let iter f t =
    let rec iter' = function
      | Node (_, ns) -> Array.iter iter' ns
      | Leaf (_, es) -> List.iter f es
      | Empty _ -> ()
    in
    iter' t.tree

  let range range t =
    let rec range' = function
      | Node (box, ns) when Box.intersects box range ->
          Iter.(of_array ns |> map range' |> concat)
      | Leaf (box, es) when Box.intersects box range ->
          Iter.of_list es |> Iter.filter (position >> Box.contains range)
      | _ -> Iter.empty
    in
    range' t.tree |> Iter.to_list

  let apply f t =
    let rec apply' = function
      | Node (box, ns) ->
          Array.map_inplace apply' ns;
          Node (box, ns)
      | Leaf (box, es) -> Leaf (box, f es)
      | t' -> t'
    in
    { t with tree = apply' t.tree }

  let filter f t = apply (Iter.of_list >> Iter.filter f >> Iter.to_list) t
  let filter_map f t = apply (List.to_seq >> Seq.filter_map f >> List.of_seq) t

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
          Iter.of_list es
          |> Iter.sort ~cmp:(fun e1 e2 ->
                 compare
                   (Point.distance (position e1) pt)
                   (Point.distance (position e2) pt))
          |> Iter.find (function
               | elt when position elt <> pt -> Some elt
               | _ -> None)
      | Empty _ -> None
    in
    search t.tree
end

module KDTree (E : ElementN) : KDTree with type elt = E.t = struct
  type elt = E.t
  type tree = Node of elt * tree * tree | Leaf of elt list | Empty
  type t = { capacity : int; dimensionality : int; tree : tree }

  let leaf points = Leaf points

  module FA = Float.Array

  let distance p1 p2 =
    FA.map2 (fun a b -> (a -. b) ** 2.) p1 p2 |> FA.fold_left ( +. ) 0.

  let compare_dim dim e1 e2 =
    let p1, p2 = (E.position e1, E.position e2) in
    FA.(compare (unsafe_get p1 dim) (unsafe_get p2 dim))

  let empty capacity dimensionality = { capacity; dimensionality; tree = Empty }

  let split idx lst =
    let rec split' i lst before =
      match lst with
      | [] -> failwith "Index out of bounds"
      | x :: xs ->
          if i == 0 then (List.rev before, x, xs)
          else split' (pred i) xs (x :: before)
    in
    split' idx lst []

  let load { capacity; dimensionality; tree = _empty } points =
    let rec load' dimension = function
      | [] -> Empty
      | points when List.length points <= capacity -> Leaf points
      | points ->
          let sorted = List.fast_sort (compare_dim dimension) points in
          let median = List.length points / 2 in
          let left, node, right = split median sorted in
          let next_dim = succ dimension mod dimensionality in
          Node (node, load' next_dim left, load' next_dim right)
    in
    { capacity; dimensionality; tree = load' 0 points }

  let insert ({ capacity; dimensionality; tree } as t) elt =
    let pos = E.position elt in
    let rec insert' depth = function
      | Node (median, left, right) ->
          let dimension = depth mod dimensionality in
          let dir =
            FA.unsafe_get pos dimension
            < FA.unsafe_get (E.position median) dimension
          in
          if dir then Node (median, insert' (succ depth) left, right)
          else Node (median, left, insert' (succ depth) right)
      | Leaf children when List.length children < capacity ->
          Leaf (elt :: children)
      | Leaf points ->
          let dimension = depth mod dimensionality in
          let sorted = List.fast_sort (compare_dim dimension) (elt :: points) in
          let median = List.length sorted / 2 in
          let left, node, right = split median sorted in
          Node
            ( node,
              insert' (succ depth) @@ leaf left,
              insert' (succ depth) @@ leaf right )
      | Empty -> Leaf [ elt ]
    in
    { t with tree = insert' 0 tree }

  let nearest t query =
    let fold_children best_opt elt =
      let pos = E.position elt in
      let dist = distance query pos in
      match best_opt with
      | Some (_, curr_dist) as opt when curr_dist < dist -> opt
      | _ -> Some (elt, dist)
    in
    let rec nearest' depth best = function
      | Empty -> best
      | Leaf elts -> List.fold_left fold_children best elts
      | Node (median, left, right) ->
          let dim = depth mod t.dimensionality in
          let point = E.position median in
          let query_d, point_d =
            FA.(unsafe_get query dim, unsafe_get point dim)
          in
          let go_left = query_d < point_d in
          let closer, further =
            if go_left then (left, right) else (right, left)
          in
          let curr_best =
            match best with
            | None -> Some (median, distance query point)
            | _ -> best
          in
          let curr_best = nearest' (succ depth) curr_best closer in

          Option.bind curr_best (fun curr ->
              if (query_d -. point_d) ** 2. < snd curr then
                nearest' (succ depth) (Some curr) further
              else Some curr)
    in

    Option.map fst @@ nearest' 0 None t.tree
end
