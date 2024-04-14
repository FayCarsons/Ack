let test_domain_max = 100.
let is_even n = Float.rem n 2. = 0.
let ( >> ) f g x = g @@ f x
(* Fn composition *)

let assert' b = assert b
(* Wrap assert for easier use *)

let time label fn =
  let open Core.Time_float in
  let start' = now () in
  fn ();
  let end' = now () in
  Printf.printf "%s took %f seconds\n" label (diff end' start' |> Span.to_sec)
;;

(* Times the fn passed and prints its duration w/ label *)

module Num = struct
  open Core
  include Float

  let mul = Float.( * )
  let div = Float.( / )
  let succ n = Float.add 1. n [@@inline]
  let pred n = Float.sub n 1. [@@inline]
end
