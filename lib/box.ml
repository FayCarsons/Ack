module type Point = Tree_intf.Point
module type Point3D = Tree_intf.Point3D
module type Box = Tree_intf.Box
module type Scalar = Tree_intf.Scalar

let in_range (start', end') x = x >= start' && x < end'

module Make (Num : Scalar) (Point : Point with type n = Num.t) :
  Box with type n = Num.t and type point = Point.t = struct
  type n = Num.t
  type point = Point.t
  type t = { min : point; max : point }

  let box (min : point) (max : point) : t = { min; max }
  let equal b1 b2 = Point.equal b1.min b2.min && Point.equal b1.max b2.max

  let midpoint { min; max } =
    let open Point in
    (min +~ max) /! two

  let split ({ min; max } as box' : t) =
    let open Point in
    let mid = midpoint box' in
    let nw = box (point min.x mid.y) (point mid.x max.y) in
    let ne = box mid max in
    let se = box (point mid.x min.y) (point max.x mid.y) in
    let sw = box min mid in
    [| nw; ne; se; sw |]

  let contains { min; max } ({ x; y } : Point.t) =
    let open Point in
    in_range (min.x, max.x) x && in_range (min.y, max.y) y

  let intersects a b =
    let open Point in
    let overlap (min_a : n) (max_a : n) (min_b : n) (max_b : n) =
      not (max_a < min_b || max_b < min_a)
    in
    overlap a.min.x a.max.x b.min.x b.max.x
    && overlap a.min.y a.max.y b.min.y b.max.y
end

module Make3D (Num : Scalar) (Point : Point3D with type n = Num.t) :
  Box with type n = Num.t and type point = Point.t = struct
  type n = Num.t
  type point = Point.t
  type t = { min : point; max : point }

  let box min max = { min; max }
  let equal b1 b2 = Point.equal b1.min b2.min && Point.equal b1.max b2.max

  let midpoint { min; max } =
    let open Point in
    (min +~ max) /! Point.two

  let split ({ min; max } as box') =
    let open Point in
    let mid = midpoint box' in
    (* Define the octants *)
    let front_top_left =
      box (point min.x min.y mid.z) (point mid.x mid.y max.z)
    in
    let front_top_right =
      box (point mid.x min.y mid.z) (point max.x mid.y max.z)
    in
    let front_bottom_left =
      box (point min.x mid.y mid.z) (point mid.x max.y max.z)
    in
    let front_bottom_right = box mid max in

    let back_top_left = box min (point mid.x mid.y mid.z) in
    let back_top_right =
      box (point mid.x min.y min.z) (point max.x mid.y mid.z)
    in
    let back_bottom_left =
      box (point min.x mid.y min.z) (point mid.x max.y mid.z)
    in
    let back_bottom_right =
      box (point mid.x mid.y min.z) (point max.x max.y mid.z)
    in

    [|
      front_top_left;
      front_top_right;
      front_bottom_left;
      front_bottom_right;
      back_top_left;
      back_top_right;
      back_bottom_left;
      back_bottom_right;
    |]

  let contains { min; max } ({ x; y; z } : point) =
    in_range (min.x, max.x) x
    && in_range (min.y, max.y) y
    && in_range (min.z, max.z) z

  let intersects a b =
    let open Point in
    let overlap (min_a : n) (max_a : n) (min_b : n) (max_b : n) =
      not (max_a < min_b || max_b < min_a)
    in
    overlap a.min.x a.max.x b.min.x b.max.x
    && overlap a.min.y a.max.y b.min.y b.max.y
    && overlap a.min.z a.max.z b.min.z b.max.z
end
