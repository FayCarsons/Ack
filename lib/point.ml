include Tree_intf

module Make2D (Num : Scalar) : Point2D with type n = Num.t = struct
  type n = Num.t
  type t = { x : n; y : n }

  let two = Num.succ Num.one
  let point x y = { x; y }
  let splat x = point x x
  let map f { x; y } = point (f x) (f y)
  let map2 f { x; y } { x = x'; y = y' } = point (f x x') (f y y')

  let iter f { x; y } =
    f x;
    f y;
    ()

  let to_tuple { x; y } = (x, y)

  (* Point -> Point arithmetic *)
  let ( +~ ) = map2 Num.add
  let ( -~ ) = map2 Num.sub
  let ( *~ ) = map2 Num.mul
  let ( /~ ) = map2 Num.div

  (* Point -> scalar arithmetic *)
  let make_point_scalar_arithmetic f p scalar = map2 f p (splat scalar)
  let ( +! ) = make_point_scalar_arithmetic Num.add
  let ( -! ) = make_point_scalar_arithmetic Num.sub
  let ( *! ) = make_point_scalar_arithmetic Num.mul
  let ( /! ) = make_point_scalar_arithmetic Num.div
end

module Make3D (Num : Scalar) : Point3D with type n = Num.t = struct
  type n = Num.t
  type t = { x : n; y : n; z : n }

  let two = Num.succ Num.one
  let point x y z = { x; y; z }
  let splat n = point n n n
  let map f { x; y; z } = point (f x) (f y) (f z)

  let map2 f { x; y; z } { x = x'; y = y'; z = z' } =
    point (f x x') (f y y') (f z z')

  let iter f { x; y; z } =
    f x;
    f y;
    f z;
    ()

  let to_tuple { x; y; z } = (x, y, z)
  let ( +~ ) = map2 Num.add
  let ( -~ ) = map2 Num.sub
  let ( *~ ) = map2 Num.mul
  let ( /~ ) = map2 Num.div
  let make_point_scalar_arithmetic f p scalar = map2 f p (splat scalar)
  let ( +! ) = make_point_scalar_arithmetic Num.add
  let ( -! ) = make_point_scalar_arithmetic Num.sub
  let ( *! ) = make_point_scalar_arithmetic Num.mul
  let ( /! ) = make_point_scalar_arithmetic Num.div
end
