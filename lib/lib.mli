module type Element2D = Tree_intf.Element2D
module type Element3D = Tree_intf.Element3D

module Quadtree (N : Tree_intf.Scalar) (E : Element2D with type n = N.t) :
  Tree_intf.Quadtree with type n = N.t and type elt = E.t

module Octree (N : Tree_intf.Scalar) (E : Element3D with type n = N.t) :
  Tree_intf.Octree with type n = N.t and type elt = E.t

module KDTree (E : Tree_intf.ElementN) : Tree_intf.KDTree with type elt = E.t
