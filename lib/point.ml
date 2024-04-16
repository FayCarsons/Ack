include Tree_intf
open Core

module Make (Num : Scalar) : Point with type n = Num.t = struct
  type n = Num.t

  type t =
    { x : n
    ; y : n
    }

  let x p = p.x
  let y p = p.y
  let z _ = failwith "Called z in 2d Point module!"
  let equal p1 p2 = Num.equal p1.x p2.x && Num.equal p1.y p2.y
  let two = Num.succ Num.one
  let point x y = { x; y }
  let splat x = point x x
  let map f { x; y } = point (f x) (f y)
  let map2 f { x; y } { x = x'; y = y' } = point (f x x') (f y y')
  let fold f acc { x; y } = f (f acc x) y

  let iter f { x; y } =
    f x;
    f y;
    ()
  ;;

  let distance p1 p2 =
    map2 Num.sub p1 p2 |> map (fun n -> Num.mul n n) |> fold Num.add Num.zero |> Num.sqrt
  ;;

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

  type t =
    { x : n
    ; y : n
    ; z : n
    }

  let x p = p.x
  let y p = p.y
  let z p = p.z
  let equal p1 p2 = Num.equal p1.x p2.x && Num.equal p1.y p2.y && Num.equal p1.z p2.z
  let two = Num.succ Num.one
  let point x y z = { x; y; z }
  let splat n = point n n n
  let map f { x; y; z } = point (f x) (f y) (f z)
  let map2 f { x; y; z } { x = x'; y = y'; z = z' } = point (f x x') (f y y') (f z z')
  let fold f acc { x; y; z } = f (f (f acc x) y) z

  let distance p1 p2 =
    map2 Num.sub p1 p2 |> map (fun n -> Num.mul n n) |> fold Num.add Num.zero |> Num.sqrt
  ;;

  let iter f { x; y; z } =
    f x;
    f y;
    f z;
    ()
  ;;

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
