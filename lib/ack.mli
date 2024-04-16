module type Element2D = Tree_intf.Element2D
module type Element3D = Tree_intf.Element3D

module Quadtree : sig
  module Make (N : Tree_intf.Scalar) (E : Element2D with type n = N.t) :
    Tree_intf.SPT with type n = N.t and type elt = E.t
end

module Octree : sig
  module Make (N : Tree_intf.Scalar) (E : Element3D with type n = N.t) :
    Tree_intf.SPT with type n = N.t and type elt = E.t
end

module KDTree : sig
  module Make (E : Tree_intf.ElementN) : Tree_intf.KDTree with type elt = E.t
end
