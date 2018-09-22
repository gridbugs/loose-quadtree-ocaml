open! Core

module Aabb = struct
  type t =
    { centre : (float * float)
    ; width : float
    ; height : float
    } [@@deriving fields, sexp_of]

  let create = Fields.create

  let create_top_left ~x ~y ~width ~height =
    let centre_x = x +. width /. 2. in
    let centre_y = y +. height /. 2. in
    { centre = (centre_x, centre_y) ; width ; height }
end

module Bounding_square = struct
  type t =
    { centre : (float * float)
    ; size : float
    } [@@deriving fields]

  let with_corner_at_origin size =
    let half_size = size /. 2. in
    { centre = (half_size, half_size) ; size }

  let of_aabb aabb =
    let centre = Aabb.centre aabb in
    let size = Float.max (Aabb.width aabb) (Aabb.height aabb) in
    { centre ; size }

  let to_aabb { centre ; size } = Aabb.create ~centre ~width:size ~height:size

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
    ; data : (Aabb.t * 'a) list
    } [@@deriving sexp_of]

  let empty_tree =
    { top_left = Leaf
    ; top_right = Leaf
    ; bottom_left = Leaf
    ; bottom_right = Leaf
    ; data = []
    }

  let rec insert t aabb value ~aabb_square ~node_square =
    if Float.(Bounding_square.size aabb_square < Bounding_square.size node_square)
    then
      match t with
      | Leaf -> Tree { empty_tree with data = [ aabb, value ] }
      | Tree tree -> Tree { tree with data = (aabb, value) :: tree.data }
    else
      let tree = match t with
        | Leaf -> empty_tree
        | Tree tree -> tree
      in
      let subnode, node_corner_square =
        let aabb_x, aabb_y = Bounding_square.centre aabb_square in
        let node_x, node_y = Bounding_square.centre node_square in
        if Float.(aabb_x < node_x)
        then
          if Float.(aabb_y < node_y)
          then tree.top_left, Bounding_square.top_left node_square
          else tree.bottom_left, Bounding_square.bottom_left node_square
        else
          if Float.(aabb_y < node_y)
          then tree.top_right, Bounding_square.top_right node_square
          else tree.bottom_right, Bounding_square.bottom_right node_square
      in
      insert subnode aabb value ~aabb_square ~node_square:node_corner_square
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

let insert t aabb value =
  let aabb_square = Bounding_square.of_aabb aabb in
  let t = grow_to_fit t aabb_square in
  let node_square = Bounding_square.with_corner_at_origin t.size in
  let root = Node.insert t.root aabb value ~aabb_square ~node_square in
  { t with root }

let%expect_test "insert" =
  let a = Aabb.create_top_left ~x:42. ~y:27. ~width:51. ~height:9. in
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
