module Quadtree
    (Num : Tree_intf.Scalar)
    (E : Tree_intf.Element2D with type n = Num.t) :
  Tree_intf.Quadtree with type n = Num.t and type elt = E.t

module Octree
    (Num : Tree_intf.Scalar)
    (E : Tree_intf.Element3D with type n = Num.t) :
  Tree_intf.Octree with type n = Num.t and type elt = E.t

module KDTree (E : Tree_intf.ElementN) : Tree_intf.KDTree with type elt = E.t
