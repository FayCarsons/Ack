let test_domain_max = 100
let is_even n = n mod 2 = 0
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
(* Times the fn passed and prints its duration w/ label *)

module Num = struct
  open Core
  include Int

  let add = Int.( + )
  let sub = Int.( - )
  let mul = Int.( * )
  let div = Int.( / )
  let sqrt n = float_of_int n |> sqrt
end
