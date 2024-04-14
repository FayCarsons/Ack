(** Scalar type used for building Point.t, Box.t, etc. This should (probably) be the same type your coordinate system uses *)
module type Scalar = sig
  type t

  val zero : t
  val one : t
  val succ : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val sqrt : t -> float
  val equal : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

(** 2D Point *)
module type Point = sig
  type n
  type t = { x : n; y : n }

  val equal : t -> t -> bool
  val two : n
  val point : n -> n -> t

  val splat : n -> t
  (** [splat n] Creates a point with all fields being n *)

  val map : (n -> n) -> t -> t
  (** [map f point] applies {i f} to fields of {i point} *)

  val map2 : (n -> n -> n) -> t -> t -> t
  (** [map2 f pt1 pt2] Applies {i f} to the fields of {i pt1} and {i pt2} pairwise. It is equivalent to: {[
      let x = f pt1.x pt2.x and y = f pt1.y pt2.y in { x; y }
      ]} *)

  val fold : ('acc -> n -> 'acc) -> 'acc -> t -> 'acc
  (** [fold f init point] Threads an accumulator through the application of {i f} to the fields of {i point} *)

  val distance : t -> t -> float
  (** [distance pt1 pt2] Returns the distance (always a {i float}) beween two points *)

  val iter : (n -> unit) -> t -> unit
  (** [iter f poin] Applies {i f} to each field of {i point}. It is assumed that {i f} is side-effectful *)

  (** Point-point arithmetic. These are equivalent to 
      {[
      let x = op pt1.x pt2.x 
      and y = op pt1.y pt2.y in  
      { x; y }
      ]}
      *)

  val ( +~ ) : t -> t -> t
  (** Point-point addition *)

  val ( -~ ) : t -> t -> t
  (** Point-point subtraction *)

  val ( *~ ) : t -> t -> t
  (** Point-point multiplication *)

  val ( /~ ) : t -> t -> t
  (** Point-point division *)

  (** Point-scalar operations, these are equivalent to 
      {[
      point_point_op point (Point.splat scalar)
      ]} *)

  val ( +! ) : t -> n -> t
  (** Point-scalar addition *)

  val ( -! ) : t -> n -> t
  (** Point-scalar subtraction *)

  val ( *! ) : t -> n -> t
  (** Point-scalar multiplication *)

  val ( /! ) : t -> n -> t
  (** Point-scalar division *)
end

module type Point3D = sig
  type n
  type t = { x : n; y : n; z : n }

  val equal : t -> t -> bool
  val two : n
  val point : n -> n -> n -> t

  val splat : n -> t
  (** [splat n] Creates a point with all fields being n *)

  val map : (n -> n) -> t -> t
  (** [map f point] applies {i f} to fields of {i point} *)

  val map2 : (n -> n -> n) -> t -> t -> t
  (** [map2 f pt1 pt2] Applies {i f} to the fields of {i pt1} and {i pt2} pairwise. It is equivalent to: 
      {[
      let x = f pt1.x pt2.x
      and y = f pt1.y pt2.y 
      and z = f pt1.z pt2.z 
      in 
      { x; y; z }
      ]} *)

  val fold : ('acc -> n -> 'acc) -> 'acc -> t -> 'acc
  (** [fold f init point] Threads an accumulator through the application of {i f} to the fields of {i point} *)

  val distance : t -> t -> float
  (** [distance pt1 pt2] Returns the distance (always a {i float}) beween two points *)

  val iter : (n -> unit) -> t -> unit
  (** [iter f poin] Applies {i f} to each field of {i point}. It is assumed that {i f} is side-effectful *)

  (** Point-point arithmetic. These are equivalent to 
      {[
      let x = op pt1.x pt2.x 
      and y = op pt1.y pt2.y 
      and z = op pt1.z pt2.z 
      in
      { x; y; z }
      ]}
      *)

  val ( +~ ) : t -> t -> t
  (** Point-point addition *)

  val ( -~ ) : t -> t -> t
  (** Point-point subtraction *)

  val ( *~ ) : t -> t -> t
  (** Point-point multiplication *)

  val ( /~ ) : t -> t -> t
  (** Point-point division *)

  (** Point-scalar operations, these are equivalent to 
      {[
      point_point_op point (Point.splat scalar)
      ]} *)

  val ( +! ) : t -> n -> t
  (** Point-scalar addition *)

  val ( -! ) : t -> n -> t
  (** Point-scalar subtraction *)

  val ( *! ) : t -> n -> t
  (** Point-scalar multiplication *)

  val ( /! ) : t -> n -> t
  (** Point-scalar division *)
end

module type Box = sig
  type t
  type n
  type point

  val equal : t -> t -> bool
  val box : point -> point -> t
  val midpoint : t -> point
  val split : t -> t array
  val contains : t -> point -> bool
  val intersects : t -> t -> bool
end

module type Element2D = sig
  type t
  type n

  val position : t -> n * n
  val equal : t -> t -> bool
end

module type Element3D = sig
  type t
  type n

  val equal : t -> t -> bool
  val position : t -> n * n * n
end

module type ElementN = sig
  type t

  val equal : t -> t -> bool
  val position : t -> float array
end

module type Quadtree = sig
  type n

  module Point : Point with type n = n
  module Box : Box with type n = n and type point = Point.t

  type elt
  type t

  val empty : Box.t -> int -> t
  (** [empty domain capacity] constructs an empty tree with leaf capacity {capacity} and spatial domain from {i domain.min} to {i domain.max} *)

  val load : t -> elt list -> t
  (** [load empty_tree elements] extends an empty tree, distributing the elements amongst its leaves *)

  val insert : t -> elt -> t
  (** [insert t elt] insert a single element into a tree *)

  val size : t -> int
  (** [size tree] returns the number of elements in  the tree *)

  val depth : t -> int
  (** [depth tree] returns the depth of the tree *)

  val remove : t -> elt -> t
  (** [remove tree elt] removes any elements that have {b deep equality} with elt from the tree *)

  val find : (elt -> bool) -> t -> elt option
  (** [find search_fn t] returns the first element for which (search_fn elt) returns true, or none *)

  val range : Box.t -> t -> elt list
  (** [range domain tree] returns all elements with a position between {i domain.min} and {i domain.max} *)

  val nearest : t -> Point.t -> elt option
  (** [nearest tree point] returns the elements nearest to {i point}*)

  val map : (elt -> elt) -> t -> t
  (** [map f tree] applies {i f} to every element of {i tree} *)

  val iter : (elt -> unit) -> t -> unit
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val mem : t -> elt -> bool
end

module type Octree = sig
  type n

  module Point : Point3D with type n = n
  module Box : Box with type n = n and type point = Point.t

  type elt
  type t

  val empty : Box.t -> int -> t
  val load : t -> elt list -> t
  val insert : t -> elt -> t
  val size : t -> int
  val depth : t -> int
  val remove : t -> elt -> t
  val find : (elt -> bool) -> t -> elt option
  val range : Box.t -> t -> elt list
  val nearest : t -> Point.t -> elt option
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val mem : t -> elt -> bool
end

module type KDTree = sig
  type t
  type elt

  val empty : int -> int -> t
  val load : t -> elt list -> t
  val insert : t -> elt -> t
  val nearest : t -> float array -> elt option
  val depth : t -> int
  val size : t -> int
end
