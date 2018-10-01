open! Core

module Bounding_rect = struct
  type t =
    { centre : (float * float)
    ; width : float
    ; height : float
    } [@@deriving fields, sexp_of]

  let create = Fields.create

  let create_with_top_left ~x ~y ~width ~height =
    let centre_x = x +. width /. 2. in
    let centre_y = y +. height /. 2. in
    { centre = (centre_x, centre_y) ; width ; height }

  let max_dimension t = Float.max t.width t.height
end

module Bounding_square = struct
  type t =
    { centre : (float * float)
    ; size : float
    } [@@deriving fields]

  let with_corner_at_origin size =
    let half_size = size /. 2. in
    { centre = (half_size, half_size) ; size }

  let of_bounding_rect bounding_rect =
    let centre = Bounding_rect.centre bounding_rect in
    let size = Bounding_rect.max_dimension bounding_rect in
    { centre ; size }

  let to_bounding_rect { centre ; size } = Bounding_rect.create ~centre ~width:size ~height:size

  let corner { centre = (x, y) ; size } x_sign y_sign =
    let offset = size /. 4. in
    let x = x +. (offset *. x_sign) in
    let y = y +. (offset *. y_sign) in
    let size = size /. 2. in
    { centre = (x, y) ; size }

  let top_left t = corner t (-1.) (-1.)
  let top_right t = corner t 1. (-1.)
  let bottom_left t = corner t (-1.) 1.
  let bottom_right t = corner t 1. 1.
end

module To_insert = struct
  type 'a t =
    { bounding_rect : Bounding_rect.t
    ; value : 'a
    } [@@deriving fields]

  let create = Fields.create

  let bounding_square t = Bounding_square.of_bounding_rect (t.bounding_rect)

  let max_dimension t = Bounding_rect.max_dimension t.bounding_rect

  let centre t = Bounding_rect.centre t.bounding_rect
end

module Node = struct
  type 'a t =
    | Leaf
    | Tree of 'a tree
    [@@deriving sexp_of]
  and 'a tree =
    { top_left : 'a t
    ; top_right : 'a t
    ; bottom_left : 'a t
    ; bottom_right : 'a t
    ; data : (Bounding_rect.t * 'a) list
    } [@@deriving sexp_of]

  let empty_tree =
    { top_left = Leaf
    ; top_right = Leaf
    ; bottom_left = Leaf
    ; bottom_right = Leaf
    ; data = []
    }

  let rec insert t ~node_square ({ To_insert.bounding_rect; value } as to_insert)  =
    if Float.(Bounding_rect.max_dimension bounding_rect < Bounding_square.size node_square)
    then
      match t with
      | Leaf -> Tree { empty_tree with data = [ bounding_rect, value ] }
      | Tree tree -> Tree { tree with data = (bounding_rect, value) :: tree.data }
    else
      let tree = match t with
        | Leaf -> empty_tree
        | Tree tree -> tree
      in
      let bounding_rect_x, bounding_rect_y = Bounding_rect.centre bounding_rect in
      let node_x, node_y = Bounding_square.centre node_square in
      if Float.(bounding_rect_x < node_x)
      then
        if Float.(bounding_rect_y < node_y)
        then
          let node_square = Bounding_square.top_left node_square in
          Tree { tree with top_left = insert tree.top_left ~node_square to_insert }
        else
          let node_square = Bounding_square.bottom_left node_square in
          Tree { tree with bottom_left = insert tree.bottom_left ~node_square to_insert }
      else
        if Float.(bounding_rect_y < node_y)
        then
          let node_square = Bounding_square.top_right node_square in
          Tree { tree with top_right = insert tree.top_right ~node_square to_insert }
        else
          let node_square = Bounding_square.bottom_right node_square in
          Tree { tree with bottom_right = insert tree.bottom_right ~node_square to_insert }
end

type 'a t =
  { root : 'a Node.t
  ; size : float
  } [@@deriving sexp_of]

let empty_with_initial_size size = { root = Node.Leaf ; size }

let rec grow_to_size { root ; size } target_size =
  if target_size <= size
  then { root ; size }
  else
    let root_tree = { Node.empty_tree with top_left = root } in
    let next = { root = Node.Tree root_tree ; size = size *. 2. } in
    grow_to_size next target_size

let grow_to_fit t bounding_square =
  let centre_max_dimension =
    let x, y = Bounding_square.centre bounding_square in
    Float.max x y
  in
  let target_size =
    Float.max centre_max_dimension (Bounding_square.size bounding_square)
  in
  grow_to_size t target_size

let insert t bounding_rect value =
  let to_insert = To_insert.create ~bounding_rect ~value in
  let t = grow_to_fit t (To_insert.bounding_square to_insert) in
  let node_square = Bounding_square.with_corner_at_origin t.size in
  let root = Node.insert t.root ~node_square to_insert in
  { t with root }

let%expect_test "insert" =
  let a = Bounding_rect.create_with_top_left ~x:42. ~y:27. ~width:51. ~height:9. in
  let t = empty_with_initial_size 32. in
  let t = insert t a "a" in
  printf !"%{sexp:string t}\n" t;
  [%expect {|
    ((root
      (Tree
       ((top_left
         (Tree
          ((top_left Leaf) (top_right Leaf) (bottom_left Leaf)
           (bottom_right Leaf) (data ()))))
        (top_right Leaf) (bottom_left Leaf) (bottom_right Leaf)
        (data ((((centre (67.5 31.5)) (width 51) (height 9)) a))))))
     (size 128)) |}]
