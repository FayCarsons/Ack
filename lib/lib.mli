module type Element = Tree_intf.Element

module T (N : Tree_intf.Scalar) (E : Element with type n = N.t) :
  Tree_intf.Quadtree with type n = N.t and type elt = E.t
