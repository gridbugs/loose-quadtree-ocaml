open! Core

module Bounding_rect : sig
  type t [@@deriving sexp_of]

  val create : centre:(float * float) -> width:float -> height:float -> t
  val create_with_nw : x:float -> y:float -> width:float -> height:float -> t
  val centre : t -> float * float
  val max_dimension : t -> float
end = struct
  type t =
    { centre : (float * float)
    ; width : float
    ; height : float
    } [@@deriving fields, sexp_of]

  let create = Fields.create

  let create_with_nw ~x ~y ~width ~height =
    let centre_x = x +. width /. 2. in
    let centre_y = y +. height /. 2. in
    { centre = (centre_x, centre_y) ; width ; height }

  let max_dimension t = Float.max t.width t.height
end

module Node_square : sig
  type t

  val create_with_corner_at_origin : size:float -> t
  val size : t -> float
  val centre : t -> float * float
  val nw : t -> t
  val ne : t -> t
  val se : t -> t
  val sw : t -> t
end = struct
  type t =
    { centre : (float * float)
    ; size : float
    } [@@deriving fields]

  let create_with_corner_at_origin ~size =
    let half_size = size /. 2. in
    { centre = (half_size, half_size) ; size }

  let corner { centre = (x, y) ; size } x_sign y_sign =
    let offset = size /. 4. in
    let x = x +. (offset *. x_sign) in
    let y = y +. (offset *. y_sign) in
    let size = size /. 2. in
    { centre = (x, y) ; size }

  let nw t = corner t (-1.) (-1.)
  let ne t = corner t 1. (-1.)
  let sw t = corner t (-1.) 1.
  let se t = corner t 1. 1.
end

module To_insert : sig
  type 'a t

  val create : bounding_rect:Bounding_rect.t -> value:'a -> 'a t
  val max_dimension : 'a t -> float
  val centre : 'a t -> (float * float)
  val bounding_rect : 'a t -> Bounding_rect.t
  val value : 'a t -> 'a
end = struct
  type 'a t =
    { bounding_rect : Bounding_rect.t
    ; value : 'a
    } [@@deriving fields]

  let create = Fields.create

  let max_dimension t = Bounding_rect.max_dimension t.bounding_rect

  let centre t = Bounding_rect.centre t.bounding_rect
end

module Node : sig
  type 'a t [@@deriving sexp_of]

  val empty : 'a t
  val empty_with_nw : 'a t -> 'a t
  val insert : 'a t -> node_square:Node_square.t -> 'a To_insert.t -> 'a t
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

  let empty = Leaf
  let empty_with_nw nw = Tree { empty_tree with nw }

  let rec insert t ~node_square to_insert  =
    let bounding_rect = To_insert.bounding_rect to_insert in
    if Float.(Bounding_rect.max_dimension bounding_rect < Node_square.size node_square)
    then
      let value = To_insert.value to_insert in
      match t with
      | Leaf -> Tree { empty_tree with data = [ bounding_rect, value ] }
      | Tree tree -> Tree { tree with data = (bounding_rect, value) :: tree.data }
    else
      let tree = match t with
        | Leaf -> empty_tree
        | Tree tree -> tree
      in
      let bounding_rect_x, bounding_rect_y = Bounding_rect.centre bounding_rect in
      let node_x, node_y = Node_square.centre node_square in
      if Float.(bounding_rect_x < node_x)
      then
        if Float.(bounding_rect_y < node_y)
        then
          let node_square = Node_square.nw node_square in
          Tree { tree with nw = insert tree.nw ~node_square to_insert }
        else
          let node_square = Node_square.sw node_square in
          Tree { tree with sw = insert tree.sw ~node_square to_insert }
      else
        if Float.(bounding_rect_y < node_y)
        then
          let node_square = Node_square.ne node_square in
          Tree { tree with ne = insert tree.ne ~node_square to_insert }
        else
          let node_square = Node_square.se node_square in
          Tree { tree with se = insert tree.se ~node_square to_insert }
end

module Quadtree : sig
  type 'a t [@@deriving sexp_of]

  val empty_with_initial_size : float -> 'a t
  val insert : 'a t -> Bounding_rect.t -> 'a -> 'a t
end = struct
  type 'a t =
    { root : 'a Node.t
    ; size : float
    } [@@deriving sexp_of]

  let empty_with_initial_size size = { root = Node.empty ; size }

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

  let insert t bounding_rect value =
    let to_insert = To_insert.create ~bounding_rect ~value in
    let t = grow_to_fit t bounding_rect in
    let node_square = Node_square.create_with_corner_at_origin ~size:t.size in
    let root = Node.insert t.root ~node_square to_insert in
    { t with root }
end

let%expect_test "insert" =
  let a = Bounding_rect.create_with_nw ~x:42. ~y:27. ~width:51. ~height:9. in
  let t = Quadtree.empty_with_initial_size 32. in
  let t = Quadtree.insert t a "a" in
  printf !"%{sexp:string Quadtree.t}\n" t;
  [%expect {|
    ((root
      (Tree
       ((nw (Tree ((nw Leaf) (ne Leaf) (sw Leaf) (se Leaf) (data ())))) (ne Leaf)
        (sw Leaf) (se Leaf)
        (data ((((centre (67.5 31.5)) (width 51) (height 9)) a))))))
     (size 128)) |}]
