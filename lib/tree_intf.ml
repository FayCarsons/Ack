(** Scalar type used for building Point.t, Box.t, etc. This should be the same number type your coordinate system uses *)
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

(** [Point] a point in 2-dimensional space *)
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

  (** [map f point] applies {b f} to fields of {b point} *)
  val map : (n -> n) -> t -> t

  (** [map2 f point_one point_two] Applies {b f} to the fields of {b pt1} and {b pt2} pairwise. It is equivalent to:
      {[
        let x = f point_one.x point_two.x
        and y = f point_one.y point_two.y in
        { x; y }
      ]} *)
  val map2 : (n -> n -> n) -> t -> t -> t

  (** [fold f init point] Threads an accumulator through the application of {b f} to the fields of {b point} *)
  val fold : ('acc -> n -> 'acc) -> 'acc -> t -> 'acc

  (** [distance pt1 pt2] Returns the distance (always a {b float}) beween two points *)
  val distance : t -> t -> float

  (** [iter f poin] Applies {b f} to each field of {b point}. It is assumed that {b f} is side-effectful *)
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

(** [Point3D] a point in 3-dimensional space *)
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

  (** [map f point] applies {b f} to fields of {b point} *)
  val map : (n -> n) -> t -> t

  (** [map2 f pt1 pt2] Applies {b f} to the fields of {b pt1} and {b pt2} pairwise. It is equivalent to:
      {[
        let x = f pt1.x pt2.x
        and y = f pt1.y pt2.y
        and z = f pt1.z pt2.z in
        { x; y; z }
      ]} *)
  val map2 : (n -> n -> n) -> t -> t -> t

  (** [fold f init point] Threads an accumulator through the application of {b f} to the fields of {b point} *)
  val fold : ('acc -> n -> 'acc) -> 'acc -> t -> 'acc

  (** [distance pt1 pt2] Returns the distance (always a {b float}) beween two points *)
  val distance : t -> t -> float

  (** [iter f poin] Applies {b f} to each field of {b point}. It is assumed that {b f} is side-effectful *)
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

  (** [midpoint box] Returns the point between {b min} and {b max} *)
  val midpoint : t -> point

  (** [split box] Splits a box into equal sub-boxes based on dimensionality. I.E. 2d box -> 4 sub-boxes, 3d -> 8, etc *)
  val split : t -> t array

  (** [contains box point] Tests whether {b point} lies within the domain of {b box} *)
  val contains : t -> point -> bool

  (**  [intesects box1 box2] Tests whether {b box1} intersects {b box2} *)
  val intersects : t -> t -> bool
end

(** Generic 2d element *)
module type Element2D = sig
  (** An elemnent with a position in 2-dimensional space *)
  type t

  type n

  val position : t -> n * n
  val equal : t -> t -> bool
end

(** Generic 3d element *)
module type Element3D = sig
  (** An element in 3-dimensional space *)
  type t

  type n

  val equal : t -> t -> bool
  val position : t -> n * n * n
end

(** Generic N-dimensional element *)
module type ElementN = sig
  (** An element in N-dimensional space *)
  type t

  val equal : t -> t -> bool
  val position : t -> float array
end

(** A Spatial Partition Tree, I.E. a Quad or Oct-tree *)
module type SPT = sig
  (** Number type for coordinate system *)
  type n

  exception Populated
  exception TreeEmpty

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

  (** A spatial partitioning tree (Quadtree, Octree) that operates within a generic coordinate system
      maintains the invariant that no leaf should hold more than {b capacity} elements *)
  type t =
    { capacity : int
    ; tree : tree
    }

  and tree =
    | Node of (Box.t * tree array)
    | Leaf of (Box.t * elt list)
    | Empty of Box.t

  (** [empty domain capacity] constructs an empty tree with leaf capacity {b capacity} and
      spatial domain from {b domain.min} to {b domain.max} *)
  val empty : Box.t -> int -> t

  (** [load empty_tree elements] bulk loads elements into an empty tree
      @raise Populated if the tree is not empty *)
  val load : t -> elt list -> t

  (** [dump tree] Return all elts in tree as a list *)
  val dump : t -> elt list

  (** [rebuild domain tree] dump elements from {b tree} and create a new tree.
      Expensive, calls should only be made when tree is small or suspected to be degenerate *)
  val rebuild : Box.t -> t -> t

  (** [insert t elt] insert a single element into a tree *)
  val insert : t -> elt -> t

  (** [size tree] counts the elements in the tree *)
  val size : t -> int

  (** [depth tree] computes the depth of the tree *)
  val depth : t -> int

  (** [remove tree elt] removes any elements that have {b deep equality} with elt from the tree *)
  val remove : t -> elt -> t

  (** [find search_fn t] returns the first element for which (search_fn elt) returns true, or none *)
  val find : (elt -> bool) -> t -> elt option

  (** [range domain tree] returns all elements with a position between {b domain.min} and {b domain.max} *)
  val range : Box.t -> t -> elt list

  (** [nearest tree point] returns the element nearest to {b point} *)
  val nearest : t -> Point.t -> elt option

  (** [map f tree] applies {b f} to every element of {b tree} *)
  val map : (elt -> elt) -> t -> t

  (** [iter f tree] Applies {b f} to every element in {b tree}. It is assumed {b f} is side-effectful *)
  val iter : (elt -> unit) -> t -> unit

  (** [filter f tree] Applies {b f} to every element in {b tree}, retaining elements for which {b f} returns true *)
  val filter : (elt -> bool) -> t -> t

  (** [filter_map f tree] Applies {b f} to every element in {b tree}, retaining elements for which {b f} returns {b Some elt} *)
  val filter_map : (elt -> elt option) -> t -> t

  (** [mem tree element] Tests whether {b elt} is a member of the set defined by {b tree}'s leaves *)
  val mem : t -> elt -> bool
end

module type KDTree = sig
  (** An N-dimensional kd-tree *)
  type t

  (** The elements contained within this kd-tree *)
  type elt

  (** [empty capacity dimensionality] creates an empty n-dimensional tree where
      N = dimensionality and maximum elements contained in a leaf = {b capacity} *)
  val empty : int -> int -> t

  (** [load tree elements] bulk load list {b elements} into {b tree} *)
  val load : t -> elt list -> t

  (** [insert tree element] insert a single element into {b tree} *)
  val insert : t -> elt -> t

  (** [nearest tree point]
      @return the element of {b tree} nearest in space to {b point} *)
  val nearest : t -> float array -> elt option

  (** [depth tree] computes the depth of {b tree} *)
  val depth : t -> int

  (** [size tree] counts the elements in {b tree} *)
  val size : t -> int

  val rebuild : t -> t
end
