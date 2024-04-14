(* SPT == Spatial Partitioning Trees *)
include Tree_intf
module MakePoint = Point.Make
module MakeRect = Box.Make
open! Core

let ( >> ) f g x = g @@ f x
let ( >>| ) = List.( >>| )

module Quadtree = struct
  module Make (Num : Scalar) (E : Element2D with type n = Num.t) :
    Quadtree with type elt = E.t and type n = Num.t = struct
    module Point : Point with type n = Num.t = MakePoint (Num)
    module Box : Box with type n = Num.t and type point = Point.t = MakeRect (Num) (Point)

    type n = Num.t
    type elt = E.t

    let position (elt : E.t) : Point.t =
      let x, y = E.position elt in
      Point.point x y
    ;;

    type t =
      { capacity : int
      ; tree : tree
      }

    and tree =
      | Node of (Box.t * tree array)
      | Leaf of (Box.t * E.t list)
      | Empty of Box.t

    let empty domain capacity = { capacity; tree = Empty domain }

    let load ({ capacity; tree } as t) elements =
      let partition children elts =
        let partition' (leaves, elts_seq) box =
          let belong, dont_belong =
            List.partition_tf ~f:(position >> Box.contains box) elts_seq
          in
          let leaf = box, belong in
          leaf :: leaves, dont_belong
        in
        let leaves, _ = Array.fold ~f:partition' ~init:([], elts) children in
        Array.of_list leaves
      in
      let rec load' (box', es') =
        if List.length es' >= capacity
        then (
          let quarters = Box.split box' in
          let children' = partition quarters es' |> Array.map ~f:load' in
          Node (box', children'))
        else Leaf (box', es')
      in
      match tree with
      | Empty bounds -> { t with tree = load' (bounds, elements) }
      | _ -> failwith "Load can only take an empty tree!"
    ;;

    let insert ({ capacity; tree } as t) elt =
      let pos = position elt in
      let rec insert' = function
        | Leaf (box, es) when List.length es <= pred capacity -> Leaf (box, elt :: es)
        | Leaf (box, es) ->
          let children = Box.split box in
          let es' = Iter.of_list es in
          let leaves =
            Array.map
              ~f:(fun box' ->
                Leaf
                  (box', Iter.to_list @@ Iter.filter (position >> Box.contains box') es'))
              children
          in
          Node (box, leaves)
        | Node (box, _) as node' when not @@ Box.contains box pos -> node'
        | Node (box, ns) -> Node (box, Array.map ~f:insert' ns)
        | Empty box -> Leaf (box, [ elt ])
      in
      { t with tree = insert' tree }
    ;;

    let size t =
      let rec size' n = function
        | Node (_, ns) -> n + (Iter.of_array ns |> Iter.map (size' 0) |> Iter.fold ( + ) 0)
        | Leaf (_, es) -> List.length es
        | Empty _ -> 0
      in
      size' 0 t.tree
    ;;

    let depth t =
      let rec depth' = function
        | Node (_, ns) ->
          let children = Iter.of_array ns in
          let max_subtree = Iter.map depth' children |> Iter.max ~lt:(fun a b -> a < b) in
          succ @@ Option.value max_subtree ~default:0
        | Leaf _ -> 1
        | Empty _ -> 0
      in
      depth' t.tree
    ;;

    let remove t elt =
      let pos = position elt in
      let rec remove' = function
        | Node (box, ns) when Box.contains box pos -> Node (box, Array.map ~f:remove' ns)
        | Leaf (box, es) when Box.contains box pos ->
          Leaf
            ( box
            , Iter.of_list es
              |> Iter.filter (fun elt' ->
                let open Poly in
                elt <> elt')
              |> Iter.to_list )
        | t -> t
      in
      { t with tree = remove' t.tree }
    ;;

    let find search { capacity = _; tree } =
      let rec find' = function
        | Node (_, ns) -> Array.find_map ~f:find' ns
        | Leaf (_, es) -> List.find ~f:search es
        | Empty _ -> None
      in
      find' tree
    ;;

    let map f t =
      let rec map' = function
        | Node (box, ns) ->
          Array.map_inplace ~f:map' ns;
          Node (box, ns)
        | Leaf (box, es) -> Leaf (box, es >>| f)
        | t -> t
      in
      { t with tree = map' t.tree }
    ;;

    let iter f t =
      let rec iter' = function
        | Node (_, ns) -> Array.iter ~f:iter' ns
        | Leaf (_, es) -> List.iter ~f es
        | Empty _ -> ()
      in
      iter' t.tree
    ;;

    let range range t =
      let rec range' = function
        | Node (box, ns) when Box.intersects box range ->
          Iter.(of_array ns |> map range' |> concat)
        | Leaf (box, es) when Box.intersects box range ->
          Iter.of_list es |> Iter.filter (position >> Box.contains range)
        | _ -> Iter.empty
      in
      range' t.tree |> Iter.to_list
    ;;

    let apply f t =
      let rec apply' = function
        | Node (box, ns) ->
          Array.map_inplace ~f:apply' ns;
          Node (box, ns)
        | Leaf (box, es) -> Leaf (box, f es)
        | t' -> t'
      in
      { t with tree = apply' t.tree }
    ;;

    let filter f t = apply (Iter.of_list >> Iter.filter f >> Iter.to_list) t
    let filter_map f t = apply (Iter.of_list >> Iter.filter_map f >> Iter.to_list) t

    let mem t elt =
      let rec mem' = function
        | Node (_, ns) -> Array.fold ~f:(fun acc node -> acc || mem' node) ~init:false ns
        | Leaf (_, es) -> List.mem es elt ~equal:(fun e e' -> Poly.( = ) e e')
        | Empty _ -> false
      in
      mem' t.tree
    ;;

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
            Float.compare
              (Point.distance (Box.midpoint @@ get_box n1) pt)
              (Point.distance (Box.midpoint @@ get_box n2) pt))
          |> Iter.find_map search
        | Leaf (_, es) ->
          Iter.of_list es
          |> Iter.sort ~cmp:(fun e1 e2 ->
            Float.compare
              (Point.distance (position e1) pt)
              (Point.distance (position e2) pt))
          |> Iter.find (fun elt ->
            if not @@ Point.equal (position elt) pt then Some elt else None)
        | Empty _ -> None
      in
      search t.tree
    ;;
  end
end

module MakePoint3D = Point.Make3D
module MakeCube = Box.Make3D

module Octree = struct
  module Make (Num : Scalar) (E : Element3D with type n = Num.t) :
    Octree with type elt = E.t and type n = Num.t = struct
    type n = Num.t

    module Point : Point3D with type n = Num.t = MakePoint3D (Num)
    module Box : Box with type n = Num.t and type point = Point.t = MakeCube (Num) (Point)

    type elt = E.t

    let position elt =
      let x, y, z = E.position elt in
      Point.point x y z
    ;;

    type t =
      { capacity : int
      ; tree : tree
      }

    and tree =
      | Node of Box.t * tree array
      | Leaf of Box.t * elt list
      | Empty of Box.t

    let empty domain capacity = { capacity; tree = Empty domain }

    let load ({ capacity; tree } as t) elements =
      let partition children elts =
        let partition' (leaves, elts_seq) box =
          let belong, dont_belong =
            List.partition_tf ~f:(position >> Box.contains box) elts_seq
          in
          let leaf = box, belong in
          leaf :: leaves, dont_belong
        in
        let leaves, _ = Array.fold ~f:partition' ~init:([], elts) children in
        Array.of_list leaves
      in
      let rec load' (box', es') =
        if List.length es' >= capacity
        then (
          let quarters = Box.split box' in
          let children' = partition quarters es' |> Array.map ~f:load' in
          Node (box', children'))
        else Leaf (box', es')
      in
      match tree with
      | Empty bounds -> { t with tree = load' (bounds, elements) }
      | _ -> failwith "Load can only take an empty tree!"
    ;;

    let insert ({ capacity; tree } as t) elt =
      let pos = position elt in
      let rec insert' = function
        | Leaf (box, es) when List.length es <= pred capacity -> Leaf (box, elt :: es)
        | Leaf (box, es) ->
          let children = Box.split box in
          let es' = Iter.of_list es in
          let leaves =
            Array.map
              ~f:(fun box' ->
                Leaf
                  (box', Iter.to_list @@ Iter.filter (position >> Box.contains box') es'))
              children
          in
          Node (box, leaves)
        | Node (box, _) as node' when not @@ Box.contains box pos -> node'
        | Node (box, ns) -> Node (box, Array.map ~f:insert' ns)
        | Empty box -> Leaf (box, [ elt ])
      in
      { t with tree = insert' tree }
    ;;

    let size t =
      let rec aux n = function
        | Node (_, ns) -> n + (Iter.of_array ns |> Iter.map (aux 0) |> Iter.fold ( + ) 0)
        | Leaf (_, es) -> List.length es
        | Empty _ -> 0
      in
      aux 0 t.tree
    ;;

    let depth t =
      let rec depth' = function
        | Node (_, ns) ->
          let children = Iter.of_array ns in
          let max_subtree = Iter.map depth' children |> Iter.max ~lt:(fun a b -> a < b) in
          succ @@ Option.value max_subtree ~default:0
        | Leaf _ -> 1
        | Empty _ -> 0
      in
      depth' t.tree
    ;;

    let remove t elt =
      let pos = position elt in
      let rec remove' = function
        | Node (box, ns) when Box.contains box pos -> Node (box, Array.map ~f:remove' ns)
        | Leaf (box, es) when Box.contains box pos ->
          Leaf
            ( box
            , Iter.of_list es
              |> Iter.filter (fun elt' -> Poly.( <> ) elt elt')
              |> Iter.to_list )
        | t -> t
      in
      { t with tree = remove' t.tree }
    ;;

    let find search { capacity = _; tree } =
      let rec find' = function
        | Node (_, ns) -> Array.find_map ~f:find' ns
        | Leaf (_, es) -> List.find ~f:search es
        | Empty _ -> None
      in
      find' tree
    ;;

    let map f t =
      let rec map' = function
        | Node (box, ns) ->
          Array.map_inplace ~f:map' ns;
          Node (box, ns)
        | Leaf (box, es) -> Leaf (box, List.map ~f es)
        | t -> t
      in
      { t with tree = map' t.tree }
    ;;

    let iter f t =
      let rec iter' = function
        | Node (_, ns) -> Array.iter ~f:iter' ns
        | Leaf (_, es) -> List.iter ~f es
        | Empty _ -> ()
      in
      iter' t.tree
    ;;

    let range range t =
      let rec range' = function
        | Node (box, ns) when Box.intersects box range ->
          Iter.(of_array ns |> map range' |> concat)
        | Leaf (box, es) when Box.intersects box range ->
          Iter.of_list es |> Iter.filter (position >> Box.contains range)
        | _ -> Iter.empty
      in
      range' t.tree |> Iter.to_list
    ;;

    let apply f t =
      let rec apply' = function
        | Node (box, ns) ->
          Array.map_inplace ~f:apply' ns;
          Node (box, ns)
        | Leaf (box, es) -> Leaf (box, f es)
        | t' -> t'
      in
      { t with tree = apply' t.tree }
    ;;

    let filter f t = apply (Iter.of_list >> Iter.filter f >> Iter.to_list) t
    let filter_map f t = apply (Iter.of_list >> Iter.filter_map f >> Iter.to_list) t

    let mem t elt =
      let rec mem' = function
        | Node (_, ns) -> Array.fold ~f:(fun acc node -> acc || mem' node) ~init:false ns
        | Leaf (_, es) -> List.mem es elt ~equal:Poly.equal
        | Empty _ -> false
      in
      mem' t.tree
    ;;

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
            Float.compare
              (Point.distance (Box.midpoint @@ get_box n1) pt)
              (Point.distance (Box.midpoint @@ get_box n2) pt))
          |> Iter.find_map search
        | Leaf (_, es) ->
          Iter.of_list es
          |> Iter.sort ~cmp:(fun e1 e2 ->
            Float.compare
              (Point.distance (position e1) pt)
              (Point.distance (position e2) pt))
          |> Iter.find (fun elt ->
            if not (Point.equal (position elt) pt) then Some elt else None)
        | Empty _ -> None
      in
      search t.tree
    ;;
  end
end
