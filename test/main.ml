open OUnit2
module Quadtree = Quadtree.Lib

module Q =
  Quadtree.Quadtree
    (Int)
    (struct
      type n = Int.t
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
let is_even n = n mod 2 == 0
let ( >> ) f g x = g @@ f x
let assert' b = assert b

let test_intersects _ =
  let box = Box.box (Point.splat 25) (Point.splat 75) in
  assert (Box.intersects box test_domain)

let test_contains _ =
  let pt = Point.point 50 50 in
  assert (Box.contains test_domain pt)

let test_size _ =
  let es = rand_es 8 100 in
  let empty = Q.empty test_domain 4 in
  let t = Q.load empty es in
  assert_equal (Q.size t) 8

let test_load_many _ =
  let es = rand_es 1000 100 in
  let empty = Q.empty test_domain 4 in
  let t = Q.load empty es in
  assert_equal (Q.size t) 1000

let test_insert _ =
  let empty = Q.empty test_domain 4 in
  let t = Q.insert empty (50, 50) in
  assert_equal (Q.size t) 1

let test_remove _ =
  let target_size = 10 in
  let es = rand_es (pred target_size) 100 in
  let not_rand = (50, 50) in
  let empty = Q.empty test_domain 4 in
  let t = Q.load empty (not_rand :: es) in
  assert_equal (Q.size t) target_size;
  let t = Q.remove t not_rand in
  assert_equal (Q.size t) (pred target_size)

let test_find _ =
  let not_target_range = 89 in
  let es = rand_es 10 not_target_range in
  let target_elt = (95, 95) in
  let empty = Q.empty test_domain 4 in
  let t = Q.load empty (target_elt :: es) in
  assert' @@ Option.is_some @@ Q.find (fun elt -> elt == target_elt) t

let test_range _ =
  let range = Box.box (Point.splat 50) (Point.splat 100) in
  let target_es = [ (80, 80); (90, 90) ] in
  let es = rand_es 100 50 in
  let empty = Q.empty test_domain 4 in
  let t = Q.load empty (target_es @ es) in
  let result_es = Q.range range t in
  assert_equal (List.length result_es) 2

let test_nearest _ =
  let es = rand_es 100 50 in
  let target_elts = [ (90, 90); (80, 80) ] in
  let empty = Q.empty test_domain 4 in
  let t = Q.load empty (target_elts @ es) in
  let target = List.hd target_elts in
  let nearest = Option.get @@ Q.nearest t @@ Point.splat (fst target) in
  assert_equal (List.nth target_elts 1) nearest

let test_map _ =
  let es = List.init 10 tuple_splat in
  let empty = Q.empty test_domain 4 in
  let t = Q.load empty es in
  let t = Q.map (fun e -> (succ @@ fst e, succ @@ snd e)) t in
  Q.iter (fun i -> assert (1 <= fst i && fst i <= 10)) t

let test_iter _ =
  let es = List.init 10 tuple_splat in
  let t = Q.load (Q.empty test_domain 4) es in
  Q.iter ((Fun.flip List.mem) es >> assert') t

let test_filter _ =
  let es = List.init 10 tuple_splat in
  let empty = Q.empty test_domain 4 in
  let t = Q.load empty es in
  let t = Q.filter (fst >> is_even) t in
  Q.iter (fst >> is_even >> assert') t

let test_filter_map _ =
  let es = List.init 10 tuple_splat in
  let t = Q.load (Q.empty test_domain 4) es in
  let t =
    Q.filter_map
      (function pair when fst pair mod 2 == 0 -> Some pair | _ -> None)
      t
  in
  Q.iter (fst >> is_even >> assert') t

let qt_suite =
  "Quadtree test suite"
  >::: [
         "Test Box.intersects" >:: test_intersects;
         "Test Box.contains" >:: test_contains;
         "Test Quadtree.load && Quadtree.size" >:: test_size;
         "Test Quadtree.load w/ 1,000 elts" >:: test_load_many;
         "Test insert" >:: test_insert;
         "Test remove" >:: test_remove;
         "Test find" >:: test_find;
         "Test range" >:: test_range;
         "Test nearest" >:: test_nearest;
         "Test map" >:: test_map;
         "Test filter" >:: test_filter;
         "Test filter_map" >:: test_filter_map;
         "Test_iter" >:: test_iter;
       ]

let _ = run_test_tt_main qt_suite
