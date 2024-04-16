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

  type t =
    { x : n
    ; y : n
    }

  val x : t -> n
  val y : t -> n
  val z : t -> n
  val equal : t -> t -> bool
  val two : n
  val point : n -> n -> t

  (** [splat n] Creates a point with all fields being n *)
  val splat : n -> t

  (** [map f point] applies {i f} to fields of {i point} *)
  val map : (n -> n) -> t -> t

  (** [map2 f pt1 pt2] Applies {i f} to the fields of {i pt1} and {i pt2} pairwise. It is equivalent to: {[
      let x = f pt1.x pt2.x and y = f pt1.y pt2.y in { x; y }
      ]} *)
  val map2 : (n -> n -> n) -> t -> t -> t

  (** [fold f init point] Threads an accumulator through the application of {i f} to the fields of {i point} *)
  val fold : ('acc -> n -> 'acc) -> 'acc -> t -> 'acc

  (** [distance pt1 pt2] Returns the distance (always a {i float}) beween two points *)
  val distance : t -> t -> float

  (** [iter f poin] Applies {i f} to each field of {i point}. It is assumed that {i f} is side-effectful *)
  val iter : (n -> unit) -> t -> unit

  (** Point-point arithmetic. These are equivalent to
      {[
        let x = op pt1.x pt2.x
        and y = op pt1.y pt2.y in
        { x; y }
      ]} *)

  (** Point-point addition *)
  val ( +~ ) : t -> t -> t

  (** Point-point subtraction *)
  val ( -~ ) : t -> t -> t

  (** Point-point multiplication *)
  val ( *~ ) : t -> t -> t

  (** Point-point division *)
  val ( /~ ) : t -> t -> t

  (** Point-scalar operations, these are equivalent to
      {[
        point_point_op point (Point.splat scalar)
      ]} *)

  (** Point-scalar addition *)
  val ( +! ) : t -> n -> t

  (** Point-scalar subtraction *)
  val ( -! ) : t -> n -> t

  (** Point-scalar multiplication *)
  val ( *! ) : t -> n -> t

  (** Point-scalar division *)
  val ( /! ) : t -> n -> t
end

module type Point3D = sig
  type n

  type t =
    { x : n
    ; y : n
    ; z : n
    }

  val x : t -> n
  val y : t -> n
  val z : t -> n
  val equal : t -> t -> bool
  val two : n
  val point : n -> n -> n -> t

  (** [splat n] Creates a point with all fields being n *)
  val splat : n -> t

  (** [map f point] applies {i f} to fields of {i point} *)
  val map : (n -> n) -> t -> t

  (** [map2 f pt1 pt2] Applies {i f} to the fields of {i pt1} and {i pt2} pairwise. It is equivalent to:
      {[
        let x = f pt1.x pt2.x
        and y = f pt1.y pt2.y
        and z = f pt1.z pt2.z in
        { x; y; z }
      ]} *)
  val map2 : (n -> n -> n) -> t -> t -> t

  (** [fold f init point] Threads an accumulator through the application of {i f} to the fields of {i point} *)
  val fold : ('acc -> n -> 'acc) -> 'acc -> t -> 'acc

  (** [distance pt1 pt2] Returns the distance (always a {i float}) beween two points *)
  val distance : t -> t -> float

  (** [iter f poin] Applies {i f} to each field of {i point}. It is assumed that {i f} is side-effectful *)
  val iter : (n -> unit) -> t -> unit

  (** Point-point arithmetic. These are equivalent to
      {[
        let x = op pt1.x pt2.x
        and y = op pt1.y pt2.y
        and z = op pt1.z pt2.z in
        { x; y; z }
      ]} *)

  (** Point-point addition *)
  val ( +~ ) : t -> t -> t

  (** Point-point subtraction *)
  val ( -~ ) : t -> t -> t

  (** Point-point multiplication *)
  val ( *~ ) : t -> t -> t

  (** Point-point division *)
  val ( /~ ) : t -> t -> t

  (** Point-scalar operations, these are equivalent to
      {[
        point_point_op point (Point.splat scalar)
      ]} *)

  (** Point-scalar addition *)
  val ( +! ) : t -> n -> t

  (** Point-scalar subtraction *)
  val ( -! ) : t -> n -> t

  (** Point-scalar multiplication *)
  val ( *! ) : t -> n -> t

  (** Point-scalar division *)
  val ( /! ) : t -> n -> t
end

(** N-dimension Axis Aligned Bounding Box (AABB) *)
module type Box = sig
  type t
  type n
  type point

  val equal : t -> t -> bool
  val box : point -> point -> t
  val get_min : t -> point
  val get_max : t -> point

  (** [midpoint box] Returns the point between {i min} and {i max} *)
  val midpoint : t -> point

  (** [split box] Splits a box into equal sub-boxes based on dimensionality. I.E. 2d box -> 4 sub-boxes, 3d -> 8, etc *)
  val split : t -> t array

  (** [contains box point] Tests whether {i point} lies within the domain of {i box} *)
  val contains : t -> point -> bool

  (**  [intesects box1 box2] Tests whether {i box1} intersects {i box2} *)
  val intersects : t -> t -> bool
end

(** Generic 2d element *)
module type Element2D = sig
  type t
  type n

  val position : t -> n * n
  val equal : t -> t -> bool
end

(** Generic 3d element *)
module type Element3D = sig
  type t
  type n

  val equal : t -> t -> bool
  val position : t -> n * n * n
end

(** Generic N-dimensional element *)
module type ElementN = sig
  type t

  val equal : t -> t -> bool
  val position : t -> float array
end

(** A Spatial Partition Tree, I.E. a Quad or Oct-tree *)
module type SPT = sig
  (** Number type for coordinate system *)
  type n

  module Point : sig
    type t

    val splat : n -> t
    val x : t -> n
    val y : t -> n
    val z : t -> n
  end

  module Box : Box with type n = n and type point = Point.t

  (** Element type *)
  type elt

  (** The tree: {[ type t = { capacity : int; tree : tree } ]} where {i capacity} 
      is the maximum number of elements in any given leaf node and {i tree} is the 
      recursive variant defining the quadtree. *)
  type t =
    { capacity : int
    ; tree : tree
    }

  and tree =
    | Node of (Box.t * tree array)
    | Leaf of (Box.t * elt list)
    | Empty of Box.t

  (** [empty domain capacity] constructs an empty tree with leaf capacity {capacity} and spatial domain from {i domain.min} to {i domain.max} *)
  val empty : Box.t -> int -> t

  (** [load empty_tree elements] extends an empty tree, distributing the elements amongst its leaves *)
  val load : t -> elt list -> t

  (** [dump tree] Return all elts in tree as a list *)
  val dump : t -> elt list

  (** [rebuild domain tree] dump elements from {i tree} and create a new tree. Should only be called on degenerate or small trees due to be expensive *)
  val rebuild : Box.t -> t -> t

  (** [insert t elt] insert a single element into a tree *)
  val insert : t -> elt -> t

  (** [size tree] returns the number of elements in  the tree *)
  val size : t -> int

  (** [depth tree] returns the depth of the tree *)
  val depth : t -> int

  (** [remove tree elt] removes any elements that have {b deep equality} with elt from the tree *)
  val remove : t -> elt -> t

  (** [find search_fn t] returns the first element for which (search_fn elt) returns true, or none *)
  val find : (elt -> bool) -> t -> elt option

  (** [range domain tree] returns all elements with a position between {i domain.min} and {i domain.max} *)
  val range : Box.t -> t -> elt list

  (** [nearest tree point] returns the elements nearest to {i point} *)
  val nearest : t -> Point.t -> elt option

  (** [map f tree] applies {i f} to every element of {i tree} *)
  val map : (elt -> elt) -> t -> t

  (** [iter f tree] Applies {i f} to every element in {i tree}. It is assumed {i f} is side-effectful *)
  val iter : (elt -> unit) -> t -> unit

  (** [filter f tree] Applies {i f} to every element in {i tree}, retaining elements for which {i f} returns true *)
  val filter : (elt -> bool) -> t -> t

  (** [filter_map f tree] Applies {i f} to every element in {i tree}, retaining elements for which {i f} returns {i Some elt} *)
  val filter_map : (elt -> elt option) -> t -> t

  (** [mem tree element] Tests whether {i elt} is a member of the set defined by {i tree}'s leaves *)
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
