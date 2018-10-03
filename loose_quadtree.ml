open! Core

module Bounding_rect : sig
  type t [@@deriving sexp_of]

  val create_with_centre : centre:(float * float) -> width:float -> height:float -> t
  val create_with_nw : nw:(float * float) -> width:float -> height:float -> t
  val centre : t -> float * float
  val max_dimension : t -> float
  val is_intersecting : t -> t -> bool
end = struct
  type t =
    { centre : (float * float)
    ; width : float
    ; height : float
    } [@@deriving fields, sexp_of]

  let create_with_centre = Fields.create

  let create_with_nw ~nw:(x, y) ~width ~height =
    let centre_x = x +. width /. 2. in
    let centre_y = y +. height /. 2. in
    { centre = (centre_x, centre_y) ; width ; height }

  let max_dimension t = Float.max t.width t.height

  let is_intersecting a b =
    let ax, ay = a.centre in
    let bx, by = b.centre in
    let dx = Float.abs (ax -. bx) in
    let dy = Float.abs (ay -. by) in
    dx *. 2. < a.width +. b.width &&
      dy *. 2. < a.height +. b.height
end

module Corner : sig
  type t = Nw | Ne | Sw | Se

  val all : t list
end = struct
  type t = Nw | Ne | Sw | Se [@@deriving enumerate]
end

module Node_square : sig
  type t

  val create_with_nw_at_origin : size:float -> t
  val size : t -> float
  val centre : t -> float * float

  (* Returns the node square with a side length which is half that
   * of the argument node square, lying in the specified corner of
   * the argument node square *)
  val quarter_in_corner : t -> Corner.t -> t

  (* Returns the bounding rectangle with identical dimensions and
   * position as the argument *)
  val to_bounding_rect : t -> Bounding_rect.t

  (* Returns the node square with a side length which is twice that
   * of the argument with the same centre as the argument *)
  val double_about_centre : t -> t
end = struct
  type t =
    { centre : (float * float)
    ; size : float
    } [@@deriving fields]

  let create_with_nw_at_origin ~size =
    let half_size = size /. 2. in
    { centre = (half_size, half_size) ; size }

  let double_about_centre t = { t with size = t.size *. 2. }

  let to_bounding_rect t =
    Bounding_rect.create_with_centre ~centre:t.centre ~width:t.size ~height:t.size

  let quarter_in_corner { centre = (x, y) ; size } corner =
    let sign_x, sign_y = match corner with
      | Corner.Nw -> (-1.), (-1.)
      | Corner.Ne -> 1., (-1.)
      | Corner.Sw -> (-1.), 1.
      | Corner.Se -> 1., 1.
    in
    let offset = size /. 4. in
    let x = x +. (offset *. sign_x) in
    let y = y +. (offset *. sign_y) in
    let size = size /. 2. in
    { centre = (x, y) ; size }
end

module Node : sig
  type 'a t [@@deriving sexp_of]

  val empty : 'a t

  (* A node which is empty except for a specified child in its nw corner *)
  val empty_with_nw : 'a t -> 'a t

  (* Insert a value into a node using a provided function [choose_corner]
   * to decide which child node to recurse into *)
  val insert
    :  'a t
    -> choose_corner:(Node_square.t -> Bounding_rect.t -> Corner.t option)
    -> node_square:Node_square.t
    -> Bounding_rect.t
    -> 'a
    -> 'a t

  (* Returns the list of all pairs of bounding rectangle and associated value
   * in the node and its descendants which intersect with a provided bounding
   * rectangle, using a provided function [can_contain_intersection] to decide
   * whether a given node square could potentially contain a bounding
   * rectangle which intersects with the provided bounding rectangle *)
  val get_intersecting
    :  'a t
    -> can_contain_intersection:(Node_square.t -> Bounding_rect.t -> bool)
    -> node_square:Node_square.t
    -> Bounding_rect.t
    -> (Bounding_rect.t * 'a) list
end = struct
  type 'a t =
    | Leaf
    | Tree of 'a tree
    [@@deriving sexp_of]
  and 'a tree =
    { nw : 'a t
    ; ne : 'a t
    ; sw : 'a t
    ; se : 'a t
    ; data : (Bounding_rect.t * 'a) list
    } [@@deriving sexp_of]

  let empty_tree =
    { nw = Leaf
    ; ne = Leaf
    ; sw = Leaf
    ; se = Leaf
    ; data = []
    }

  let get_subnode tree corner =
    match corner with
    | Corner.Nw -> tree.nw
    | Corner.Ne -> tree.ne
    | Corner.Sw -> tree.sw
    | Corner.Se -> tree.se

  let set_subnode tree corner t =
    match corner with
    | Corner.Nw -> { tree with nw = t }
    | Corner.Ne -> { tree with ne = t }
    | Corner.Sw -> { tree with sw = t }
    | Corner.Se -> { tree with se = t }

  let empty = Leaf
  let empty_with_nw nw = Tree { empty_tree with nw }

  let rec insert t ~choose_corner ~node_square bounding_rect value =
    let tree = match t with
      | Leaf -> empty_tree
      | Tree tree -> tree
    in
    match choose_corner node_square bounding_rect with
    | None ->
      Tree { tree with data = (bounding_rect, value) :: tree.data }
    | Some corner ->
      let node_square = Node_square.quarter_in_corner node_square corner in
      let subnode = get_subnode tree corner in
      let subnode = insert subnode ~choose_corner ~node_square bounding_rect value in
      Tree (set_subnode tree corner subnode)

  let rec get_intersecting t ~can_contain_intersection ~node_square bounding_rect =
    match t with
    | Leaf -> []
    | Tree tree ->
      let data_for_current_node = List.filter
        tree.data
        ~f:(fun (bounding_rect', _data) ->
          Bounding_rect.is_intersecting bounding_rect bounding_rect')
      in
      let data_for_descendant_nodes = List.map Corner.all ~f:(fun corner ->
        let node_square = Node_square.quarter_in_corner node_square corner in
        if can_contain_intersection node_square bounding_rect
        then
          let subnode = get_subnode tree corner in
          get_intersecting subnode ~can_contain_intersection ~node_square bounding_rect
        else [])
      in
      List.concat (data_for_current_node :: data_for_descendant_nodes)
end

module Loose_quadtree : sig
  type 'a t [@@deriving sexp_of]

  val empty_with_initial_size : float -> 'a t
  val insert : 'a t -> Bounding_rect.t -> 'a -> 'a t
  val get_intersecting : 'a t -> Bounding_rect.t -> (Bounding_rect.t * 'a) list
end = struct
  type 'a t =
    { root : 'a Node.t
    ; size : float
    } [@@deriving sexp_of]

  let empty_with_initial_size size = { root = Node.empty ; size }

  let make_node_square t = Node_square.create_with_nw_at_origin ~size:t.size

  let rec grow_to_size { root ; size } target_size =
    if target_size <= size
    then { root ; size }
    else
      let root = Node.empty_with_nw root in
      let next = { root ; size = size *. 2. } in
      grow_to_size next target_size

  let grow_to_fit t bounding_rect =
    let centre_max_dimension =
      let x, y = Bounding_rect.centre bounding_rect in
      Float.max x y
    in
    let target_size =
      Float.max centre_max_dimension (Bounding_rect.max_dimension bounding_rect)
    in
    grow_to_size t target_size

  let choose_corner node_square bounding_rect =
    if Bounding_rect.max_dimension bounding_rect >. Node_square.size node_square /. 2. then
      None
    else
      let bounding_rect_x, bounding_rect_y = Bounding_rect.centre bounding_rect in
      let node_x, node_y = Node_square.centre node_square in
      let corner = if bounding_rect_x <. node_x
      then
        if bounding_rect_y <. node_y
        then Corner.Nw
        else Corner.Sw
      else
        if bounding_rect_y <. node_y
        then Corner.Ne
        else Corner.Se
      in
      Some corner

  let insert t bounding_rect value =
    let t = grow_to_fit t bounding_rect in
    let node_square = make_node_square t in
    let root = Node.insert t.root ~choose_corner ~node_square bounding_rect value in
    { t with root }

  let can_contain_intersection node_square bounding_rect =
    let node_bounding_rect =
      Node_square.to_bounding_rect (Node_square.double_about_centre node_square)
    in
    Bounding_rect.is_intersecting bounding_rect node_bounding_rect

  let get_intersecting t bounding_rect =
    let node_square = make_node_square t in
    Node.get_intersecting t.root ~can_contain_intersection ~node_square bounding_rect
end
