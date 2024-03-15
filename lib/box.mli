module Make
    (Num : Tree_intf.Scalar)
    (Point : Tree_intf.Point with type n = Num.t) :
  Tree_intf.Box with type n = Num.t and type point = Point.t

module Make3D
    (Num : Tree_intf.Scalar)
    (Point : Tree_intf.Point3D with type n = Num.t) :
  Tree_intf.Box with type n = Num.t and type point = Point.t
