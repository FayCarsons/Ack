open OUnit2
open Core
open Util

module O =
  Ack.Octree.Make
    (Num)
    (struct
      type n = Num.t
      type t = n * n * n

      let equal : t -> t -> bool =
        Tuple3.equal ~eq1:Num.equal ~eq2:Num.equal ~eq3:Num.equal
      ;;

      let position = Fun.id
    end)

module Box3 = O.Box
module Point3 = O.Point

let test_domain = Box3.box (Point3.splat 0) (Point3.splat test_domain_max)
let tuple_splat n = n, n, n

let rand_es n max =
  List.init n ~f:(fun _ -> Random.int max, Random.int max, Random.int max)
;;

let test_intersects _ =
  let box = Box3.box (Point3.splat 25) (Point3.splat 75) in
  assert (Box3.intersects test_domain box)
;;

let test_contains _ =
  let pt = Point3.splat 50 in
  assert (Box3.contains test_domain pt)
;;

let test_load_size _ =
  let es = rand_es 1_000 100 in
  let t = O.load (O.empty test_domain 32) es in
  assert (Int.equal (O.size t) 1_000)
;;

let test_load_many _ =
  let op () =
    let size = 100_000 in
    let es = rand_es size 100 in
    let t = O.load (O.empty test_domain 64) es in
    assert_equal (O.size t) size
  in
  time "Octree load 100k" op
;;

let test_load_lots _ =
  let op () =
    let size = 1_000_000 in
    let es = rand_es size 100 in
    let t = O.load (O.empty test_domain 256) es in
    assert_equal (O.size t) size
  in
  time "Octree load 1mil" op
;;

let test_mem _ =
  let target = tuple_splat 50 in
  let t = O.load (O.empty test_domain 4) [ target ] in
  assert (O.mem t target)
;;

(* Test inserting a single elt *)
let test_insert _ =
  let empty = O.empty test_domain 4 in
  let t = O.insert empty (tuple_splat 50) in
  assert_equal (O.size t) 1
;;

(* Test removing a single elt from a 1k elt tree *)
let test_remove _ =
  let target_size = 1_000 in
  let es = rand_es (pred target_size) 100 in
  let not_rand = tuple_splat 50 in
  let empty = O.empty test_domain 32 in
  let t = O.load empty @@ (not_rand :: es) in
  assert_equal (O.size t) target_size;
  let t = O.remove t not_rand in
  assert_equal (O.size t) (pred target_size)
;;

(* Test finding a single elt in a 1k elt tree *)
let test_find _ =
  let not_target_range = 90 in
  let es = rand_es 1_000 not_target_range in
  let target_elt = tuple_splat 95 in
  let t = O.load (O.empty test_domain 32) @@ (target_elt :: es) in
  assert' @@ Option.is_some @@ O.find (fun elt -> Poly.equal elt target_elt) t
;;

(* Test collecting all elements within a range, from within a 1k elt tree *)
let test_range _ =
  let range = Box3.box (Point3.splat 50) (Point3.splat 100) in
  let target_es = [ tuple_splat 80; tuple_splat 90 ] in
  let es = rand_es 1_000 50 in
  let t = O.load (O.empty test_domain 32) @@ target_es @ es in
  let result_es = O.range range t in
  assert_equal (List.length result_es) 2
;;

(* Test finding the nearest elt to a query point *)
let test_nearest _ =
  let es = rand_es 1_000 50 in
  let target, nearest = tuple_splat 90, tuple_splat 80 in
  let t = O.load (O.empty test_domain 32) @@ (target :: nearest :: es) in
  let nearest' = fst3 target |> Point3.splat |> O.nearest t |> Option.value_exn in
  assert_equal nearest nearest'
;;

(* Test mapping over elts in 1k elt tree *)
let test_map _ =
  let es = List.init 1_000 ~f:tuple_splat in
  let t = O.load (O.empty test_domain 32) es in
  let t = O.map (fun (x, _, _) -> tuple_splat @@ succ x) t in
  O.iter (fun (x, _, _) -> assert' (1 <= x && x <= 1_000)) t
;;

(* Test iterating over elts *)
let test_iter _ =
  let es = List.init 1_000 ~f:tuple_splat in
  let t = O.load (O.empty test_domain 32) es in
  O.iter (fun (x, _, _) -> assert (0 <= x && x <= 1_000)) t
;;

(* Test filtering elts *)
let test_filter _ =
  let es = List.init 1_000 ~f:tuple_splat in
  let t = O.load (O.empty test_domain 32) es in
  let t = O.filter (fst3 >> is_even) t in
  O.iter (fst3 >> is_even >> assert') t
;;

(* Test filter_map-ing elts *)
let test_filter_map _ =
  let es = List.init 1_000 ~f:tuple_splat in
  let t = O.load (O.empty test_domain 32) es in
  let t =
    O.filter_map
      (function
       | pair when is_even @@ fst3 pair -> Some pair
       | _ -> None)
      t
  in
  O.iter (fst3 >> is_even >> assert') t
;;

let ot_suite =
  "Octree test suite"
  >::: [ "Octree intersects" >:: test_intersects
       ; "Octree contains" >:: test_contains
       ; "Octree load + size" >:: test_load_size
       ; "Octree load 100_000" >:: test_load_many
       ; "Octree load 1_000_000" >:: test_load_lots
       ; "Octree test mem" >:: test_mem
       ; "Octree test insert" >:: test_insert
       ; "Octree test remove" >:: test_remove
       ; "Octree test find" >:: test_find
       ; "Octree test range" >:: test_range
       ; "Octree test nearest" >:: test_nearest
       ; "Octree test map" >:: test_map
       ; "Octree test filter" >:: test_filter
       ; "Octree test filter_map" >:: test_filter_map
       ; "Octree test iter" >:: test_iter
       ]
;;

let run () = run_test_tt_main ot_suite
