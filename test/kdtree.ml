open OUnit2
open Core

module Elt = struct
  type t =
    { name : string
    ; position : float array
    }

  let position t = t.position
  let equal t1 t2 = String.equal t1.name t2.name
  let elt name position = { name; position }
  let _name { name; _ } = name
end

module K = Ack.KDTree.Make (Elt)

let random_str () = String.init (succ @@ Random.int 10) ~f:char_of_int
let rand_elt max' = Elt.elt (random_str ()) (Array.init 2 ~f:(fun _ -> Random.float max'))
let rand_es n max' = List.init n ~f:(fun _ -> rand_elt max')

let test_empty _ =
  let t = K.empty 1 2 in
  assert_equal 0 @@ K.size t
;;

let test_size _ =
  let es = rand_es 100 100. in
  let t = K.load (K.empty 2 2) es in
  assert_equal (K.size t) 100
;;

let test_depth _ =
  let es = rand_es 3 100. in
  let t = K.load (K.empty 2 2) es in
  assert_equal (K.depth t) 2
;;

let kd_suite =
  "KDTree test suite"
  >::: [ "Test empty" >:: test_empty
       ; "Test size" >:: test_size
       ; "Test depth" >:: test_depth
       ]
;;

let run () = run_test_tt_main kd_suite
