open OUnit2
module Quadtree = Ack.Quadtree

module Num = struct
  include Int

  let sqrt n = float_of_int n |> sqrt
end

module Q =
  Quadtree
    (Num)
    (struct
      type n = Num.t
      type t = n * n

      let position = Fun.id
    end)

module Point = Q.Point
module Box = Q.Box

let test_domain_max = 100
let test_domain = Box.box (Point.splat 0) (Point.splat test_domain_max)
(* Test box/domain, in range 0-100 *)

let rand_es n max = List.init n (fun _ -> (Random.int max, Random.int max))
(* Random elt generator *)

let tuple_splat n = (n, n)
(* Elt from int *)

let is_even n = n mod 2 == 0
let ( >> ) f g x = g @@ f x
(* Fn composition *)

let assert' b = assert b
(* Wrap assert for easier use *)

let time label fn =
  let start' = Unix.gettimeofday () in
  fn ();
  let end' = Unix.gettimeofday () in
  Printf.printf "%s took %f seconds\n" label (end' -. start')

(* Tests whether two boxes intersect *)
let test_intersects _ =
  let box = Box.box (Point.splat 25) (Point.splat 75) in
  assert (Box.intersects box test_domain)

(* Tests whether a point is contained in a box *)
let test_contains _ =
  let pt = Point.splat 50 in
  assert (Box.contains test_domain pt)

(* Test size of tree *)
let test_size _ =
  let size = 8 in
  let es = rand_es size 100 in
  let t = Q.load (Q.empty test_domain 4) es in
  assert_equal (Q.size t) size

(* Test loading a lot of elements -- TODO optimize `Q.load` *)
let test_load_many _ =
  let op () =
    let size = 100_000 in
    let es = rand_es size 100 in
    let t = Q.load (Q.empty test_domain 64) es in
    assert_equal (Q.size t) size
  in
  time "TEST LOAD 100_000" op

let test_load_lots _ =
  let op () =
    let size = 1_000_000 in
    let es = rand_es size 100 in
    let t = Q.load (Q.empty test_domain 256) es in
    assert_equal (Q.size t) size
  in
  time "TEST LOAD 1_000_000" op

(* Test inserting a single elt *)
let test_insert _ =
  let empty = Q.empty test_domain 4 in
  let t = Q.insert empty (tuple_splat 50) in
  assert_equal (Q.size t) 1

(* Test removing a single elt from a 1k elt tree *)
let test_remove _ =
  let target_size = 1_000 in
  let es = rand_es (pred target_size) 100 in
  let not_rand = (50, 50) in
  let empty = Q.empty test_domain 32 in
  let t = Q.load empty @@ (not_rand :: es) in
  assert_equal (Q.size t) target_size;
  let t = Q.remove t not_rand in
  assert_equal (Q.size t) (pred target_size)

(* Test finding a single elt in a 1k elt tree *)
let test_find _ =
  let not_target_range = 90 in
  let es = rand_es 1_000 not_target_range in
  let target_elt = (95, 95) in
  let t = Q.load (Q.empty test_domain 32) @@ (target_elt :: es) in
  assert' @@ Option.is_some @@ Q.find (fun elt -> elt == target_elt) t

(* Test collecting all elements within a range, from within a 1k elt tree *)
let test_range _ =
  let range = Box.box (Point.splat 50) (Point.splat 100) in
  let target_es = [ (80, 80); (90, 90) ] in
  let es = rand_es 1_000 50 in
  let empty = Q.empty test_domain 32 in
  let t = Q.load empty @@ target_es @ es in
  let result_es = Q.range range t in
  assert_equal (List.length result_es) 2

(* Test finding the nearest elt to a query point *)
let test_nearest _ =
  let es = rand_es 1_000 50 in
  let target, nearest = ((90, 90), (80, 80)) in
  let empty = Q.empty test_domain 32 in
  let t = Q.load empty @@ (target :: nearest :: es) in
  let nearest' = Option.get @@ Q.nearest t @@ Point.splat (fst target) in
  assert_equal nearest nearest'

(* Test mapping over elts in 1k elt tree *)
let test_map _ =
  let es = List.init 1_000 tuple_splat in
  let t = Q.load (Q.empty test_domain 32) es in
  let t = Q.map (fun (x, _) -> tuple_splat @@ succ x) t in
  Q.iter (fun (x, _) -> assert (1 <= x && x <= 1_000)) t

(* Test iterating over elts *)
let test_iter _ =
  let es = List.init 1_000 tuple_splat in
  let t = Q.load (Q.empty test_domain 32) es in
  Q.iter (fst >> (fun i -> 0 <= i && i <= 1_000) >> assert') t

(* Test filtering elts *)
let test_filter _ =
  let es = List.init 1_000 tuple_splat in
  let t = Q.load (Q.empty test_domain 32) es in
  let t = Q.filter (fst >> is_even) t in
  Q.iter (fst >> is_even >> assert') t

(* Test filter_map-ing elts *)
let test_filter_map _ =
  let es = List.init 1_000 tuple_splat in
  let t = Q.load (Q.empty test_domain 32) es in
  let t =
    Q.filter_map
      (function pair when is_even @@ fst pair -> Some pair | _ -> None)
      t
  in
  Q.iter (fst >> is_even >> assert') t

let test_mem _ =
  let target = tuple_splat 50 in
  let t = Q.load (Q.empty test_domain 4) [ target ] in
  assert (Q.mem t target)

let qt_suite =
  "Quadtree test suite"
  >::: [
         "Test Box.intersects" >:: test_intersects;
         "Test Box.contains" >:: test_contains;
         "Test Quadtree.load && Quadtree.size" >:: test_size;
         "Test Quadtree.load w/ 100,000 elts" >:: test_load_many;
         "Test Quadtree.load w/ 1,000,000 elts" >:: test_load_lots;
         "Test insert" >:: test_insert;
         "Test remove" >:: test_remove;
         "Test find" >:: test_find;
         "Test range" >:: test_range;
         "Test nearest" >:: test_nearest;
         "Test map" >:: test_map;
         "Test filter" >:: test_filter;
         "Test filter_map" >:: test_filter_map;
         "Test iter" >:: test_iter;
         "Test mem" >:: test_mem;
       ]

let _ = run_test_tt_main qt_suite

module O =
  Ack.Octree
    (Num)
    (struct
      type n = Num.t
      type t = n * n * n

      let position = Fun.id
    end)

module Box3 = O.Box
module Point3 = O.Point

let test_domain = Box3.box (Point3.splat 0) (Point3.splat test_domain_max)
let tuple_splat n = (n, n, n)

let rand_es n max =
  List.init n (fun _ -> (Random.int max, Random.int max, Random.int max))

let fst (a, _, _) = a

let test_intersects _ =
  let box = Box3.box (Point3.splat 25) (Point3.splat 75) in
  assert (Box3.intersects test_domain box)

let test_contains _ =
  let pt = Point3.splat 50 in
  assert (Box3.contains test_domain pt)

let test_load_size _ =
  let es = rand_es 1_000 100 in
  let t = O.load (O.empty test_domain 32) es in
  assert (O.size t == 1_000)

let test_load_many _ =
  let op () =
    let size = 100_000 in
    let es = rand_es size 100 in
    let t = O.load (O.empty test_domain 64) es in
    assert_equal (O.size t) size
  in
  time "Octree load 100k" op

let test_load_lots _ =
  let op () =
    let size = 1_000_000 in
    let es = rand_es size 100 in
    let t = O.load (O.empty test_domain 256) es in
    assert_equal (O.size t) size
  in
  time "Octree load 1mil" op

let test_mem _ =
  let target = tuple_splat 50 in
  let t = O.load (O.empty test_domain 4) [ target ] in
  assert (O.mem t target)

(* Test inserting a single elt *)
let test_insert _ =
  let empty = O.empty test_domain 4 in
  let t = O.insert empty (tuple_splat 50) in
  assert_equal (O.size t) 1

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

(* Test finding a single elt in a 1k elt tree *)
let test_find _ =
  let not_target_range = 90 in
  let es = rand_es 1_000 not_target_range in
  let target_elt = tuple_splat 95 in
  let t = O.load (O.empty test_domain 32) @@ (target_elt :: es) in
  assert' @@ Option.is_some @@ O.find (fun elt -> elt == target_elt) t

(* Test collecting all elements within a range, from within a 1k elt tree *)
let test_range _ =
  let range = Box3.box (Point3.splat 50) (Point3.splat 100) in
  let target_es = [ tuple_splat 80; tuple_splat 90 ] in
  let es = rand_es 1_000 50 in
  let t = O.load (O.empty test_domain 32) @@ target_es @ es in
  let result_es = O.range range t in
  assert_equal (List.length result_es) 2

(* Test finding the nearest elt to a query point *)
let test_nearest _ =
  let es = rand_es 1_000 50 in
  let target, nearest = (tuple_splat 90, tuple_splat 80) in
  let t = O.load (O.empty test_domain 32) @@ (target :: nearest :: es) in
  let nearest' = Option.get @@ O.nearest t @@ Point3.splat @@ fst target in
  assert_equal nearest nearest'

(* Test mapping over elts in 1k elt tree *)
let test_map _ =
  let es = List.init 1_000 tuple_splat in
  let t = O.load (O.empty test_domain 32) es in
  let t = O.map (fun (x, _, _) -> tuple_splat @@ succ x) t in
  O.iter (fun (x, _, _) -> assert' (1 <= x && x <= 1_000)) t

(* Test iterating over elts *)
let test_iter _ =
  let es = List.init 1_000 tuple_splat in
  let t = O.load (O.empty test_domain 32) es in
  O.iter (fun (x, _, _) -> assert (0 <= x && x <= 1_000)) t

(* Test filtering elts *)
let test_filter _ =
  let es = List.init 1_000 tuple_splat in
  let t = O.load (O.empty test_domain 32) es in
  let t = O.filter (fst >> is_even) t in
  O.iter (fst >> is_even >> assert') t

(* Test filter_map-ing elts *)
let test_filter_map _ =
  let es = List.init 1_000 tuple_splat in
  let t = O.load (O.empty test_domain 32) es in
  let t =
    O.filter_map
      (function pair when is_even @@ fst pair -> Some pair | _ -> None)
      t
  in
  O.iter (fst >> is_even >> assert') t

let ot_suite =
  "Octree test suite"
  >::: [
         "Octree intersects" >:: test_intersects;
         "Octree contains" >:: test_contains;
         "Octree load + size" >:: test_load_size;
         "Octree load 100_000" >:: test_load_many;
         "Octree load 1_000_000" >:: test_load_lots;
         "Octree test mem" >:: test_mem;
         "Octree test insert" >:: test_insert;
         "Octree test remove" >:: test_remove;
         "Octree test find" >:: test_find;
         "Octree test range" >:: test_range;
         "Octree test nearest" >:: test_nearest;
         "Octree test map" >:: test_map;
         "Octree test filter" >:: test_filter;
         "Octree test filter_map" >:: test_filter_map;
         "Octree test iter" >:: test_iter;
       ]

let _ = run_test_tt_main ot_suite

module Elt = struct
  type t = { name : string; position : floatarray }

  let position t = t.position
  let elt name position = { name; position }
  let _name { name; _ } = name
end

module K = Ack.KDTree (Elt)
module FA = Float.Array

let random_str () =
  Seq.init (succ @@ Random.int 10) char_of_int |> String.of_seq

let rand_elt max' =
  Elt.elt (random_str ()) (FA.init 2 @@ fun _ -> Random.float max')

let rand_es n max' = List.init n (fun _ -> rand_elt max')

let test_empty _ =
  let t = K.empty 1 2 in
  assert_equal 0 @@ K.size t

let test_size _ =
  let es = rand_es 100 100. in
  let t = K.load (K.empty 2 2) es in
  assert_equal (K.size t) 100

let test_depth _ =
  let es = rand_es 3 100. in
  let t = K.load (K.empty 2 2) es in
  assert_equal (K.depth t) 2

let kd_suite =
  "KDTree test suite"
  >::: [
         "Test empty" >:: test_empty;
         "Test size" >:: test_size;
         "Test depth" >:: test_depth;
       ]

let _ = run_test_tt_main kd_suite
