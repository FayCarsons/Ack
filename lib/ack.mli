open Tree_intf

module Quadtree : sig
  module Make (N : Scalar) (E : Element2D with type n = N.t) :
    SPT with type n = N.t and type elt = E.t
end

module Octree : sig
  module Make (N : Scalar) (E : Element3D with type n = N.t) :
    SPT with type n = N.t and type elt = E.t
end

module KDTree : sig
  module Make (E : ElementN) : KDTree with type elt = E.t
end
