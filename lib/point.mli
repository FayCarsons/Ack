module Make (Num : Tree_intf.Scalar) : Tree_intf.Point with type n = Num.t
module Make3D (Num : Tree_intf.Scalar) : Tree_intf.Point3D with type n = Num.t
