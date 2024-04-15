open Core

module Num = struct
  open Core
  include Float

  let mul = Float.( * )
  let div = Float.( / )
  let succ n = Float.add 1. n
end

module Q =
  Ack.Quadtree.Make
    (Num)
    (struct
      type n = Num.t
      type t = n * n

      let position = Fn.id
      let equal = Tuple2.equal ~eq1:Num.equal ~eq2:Num.equal
    end)

open Q

let to_rect (box : Box.t) =
  let ({ x; y } : Point.t) = Box.midpoint box in
  let x, y = Tuple2.map ~f:Num.to_int (x, y) in
  let ({ x = minx; y = miny } : Point.t) = Box.get_min box in
  let ({ x = maxx; y = maxy } : Point.t) = Box.get_max box in
  let w = maxx -. minx |> Num.to_int in
  let h = maxy -. miny |> Num.to_int in
  Joy.(rectangle ~c:(point x y) w h)
;;

let circle_of_point pos =
  let x, y = Tuple2.map ~f:Num.to_int pos in
  Joy.(circle ~c:(point x y) 1 |> with_fill (255, 1, 1) |> no_stroke)
;;

let rec to_shapes : Q.tree -> Joy.shape list = function
  | Node (_, children) -> Array.to_list children |> List.concat_map ~f:to_shapes
  | Leaf (box, es) ->
    let rect = to_rect box in
    let pts = List.map ~f:circle_of_point es in
    rect :: pts
  | Empty _ -> failwith "unreachable"
;;

let size = 1200.
let half_size = size /. 2.

let rand_elt () =
  let x = Random.float size -. half_size
  and y = Random.float size -. half_size in
  x, y
;;

let centered_elt (center : float * float) _ : float * float =
  let offset () = Random.float 100. -. 50. in
  Tuple2.map2 ~f:( +. ) center (offset (), offset ())
;;

let cluster _ =
  let center = rand_elt () in
  List.init (succ @@ Random.int 128) ~f:(centered_elt center)
;;

let size =
  let isize = int_of_float size in
  isize, isize
;;

let () =
  Joy.init ~size ();
  let t = empty (Box.box (Point.splat @@ -.half_size) (Point.splat half_size)) 16 in
  let es = List.init 100 ~f:cluster |> List.concat in
  let t = load t es in
  let shapes = to_shapes t.tree in
  Joy.show shapes;
  Joy.write ~filename:"quadtree.png" ()
;;
