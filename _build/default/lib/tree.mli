module Make
    (Num : Tree_intf.Scalar)
    (E : Tree_intf.Element with type n = Num.t) :
  Tree_intf.Quadtree with type n = Num.t and type elt = E.t
