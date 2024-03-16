include Tree_intf

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

  let depth t =
    let rec depth' = function
      | Node (_, left, right) -> succ @@ max (depth' left) (depth' right)
      | Leaf _ -> 1
      | Empty -> 0
    in
    depth' t.tree

  let size t =
    let rec size' = function
      | Node (_, left, right) -> succ @@ (size' left + size' right)
      | Leaf elts -> List.length elts
      | Empty -> 0
    in
    size' t.tree
end
