module type Monoid = sig
  type t
  (* Associative *)
  val reduce : t -> t -> t
  (* forall x : t, reduce x identity = reduce identity x = x *)
  val identity : t
end

module IntPlus = struct
  type t = int
  let reduce x y = x + y
  let identity = 0
end

module IntMax = struct
  type t = int
  let reduce x y = max x y
  let identity = Int.min_int
end

module Seglist (M : Monoid)
 : sig
   type t
   val make : M.t array -> t
   val length : t -> int
   val update_in_place : (M.t -> M.t) -> int -> t -> t
   val update : int -> M.t -> t -> t
   val total_sum : t -> M.t
   val interval_sum : int -> int -> t -> M.t
 end
 = struct
  type elem = M.t
  type tree = Leaf of elem | Node of {left : tree; right : tree; acc : elem; size : int}

  let total_sum = function
  | Leaf x -> x
  | Node{acc} -> acc

  let length = function
  | Leaf _ -> 1
  | Node{size} -> size

  type t = tree

  let make elems =
    if (Array.length elems) = 0 then raise (Failure "Empty seglist not supported") else
    (* Build tree for range [i,j) *)
    let rec build i j =
      if i+1 = j then Leaf (elems.(i)) else
      let mid = (i+j)/2 in
      let left = build i mid in
      let right = build mid j in
      Node {left; right; size = j-i; acc = M.reduce (total_sum left) (total_sum right)}
    in
    build 0 (Array.length elems)

  let update_in_place f =
    let rec update' i = function
    | Leaf old -> Leaf (f old)
    | Node{left;right;size;acc} ->
        let left_size = length left in
        if i < left_size then
          let left' = update' i left in
          Node{left = left'; right; size; acc = M.reduce (total_sum left') (total_sum right)}
        else
          let right' = update' (i-left_size) right in
          Node{left; right = right'; size; acc = M.reduce (total_sum left) (total_sum right')}
    in
  fun i tree ->
    update' i tree

  let update i newval tree = update_in_place (fun _ -> newval) i tree

  let interval_sum =
    let rec isum_tree i j = function
      | Leaf x -> assert (i = 0 && j = 1) ; x
      | Node {left; right; size; acc} ->
          (* Short circuit if interval is entire subtree *)
          if i = 0 && j = size then acc else
          let left_size = length left in
          if j <= left_size then (* Entire interval on left *)
            isum_tree i j left
          else if 0 <= i-left_size then (* Entire interval on right *)
            isum_tree (i-left_size) (j-left_size) right
          else (* Interval split across both sides *)
            M.reduce (isum_tree i left_size left) (isum_tree 0 (j-left_size) right)
    in fun i j tree ->
      assert (0 <= i && i < j && j <= length tree);
      isum_tree i j tree
end

module SeglistMutable (M : Monoid)
: sig
  type t
  val make : M.t array -> t
  val update_in_place : (M.t -> M.t) -> int -> t -> unit
  val update : int -> M.t -> t -> unit
  val total_sum : t -> M.t
  val interval_sum : int -> int -> t -> M.t
end
 = struct
  type elem = M.t
  type t = int * elem array

  let lchild i = 2*i
  let rchild i = 2*i+1
  let parent i = i/2
  let is_lchild i = (i mod 2 = 0)

  let make elems =
    (* Counts number of right shifts before we hit 0 *)
    let rec log x acc = if x <= 1 then acc else log (x/2) (acc+1) in
    (* Ensure pow2_size is 2^x >= ||elems|| *)
    let pow2_size = 1 lsl log (Array.length elems - 1) 1 in
    let end_elems = pow2_size + Array.length elems in
    (* Initialize array, copying in old elements *)
    let arr = Array.init (2*pow2_size)
      (fun i -> if i < pow2_size || i >= end_elems then M.identity else elems.(i-pow2_size))
      in
    (* Recursively initialize all the non-leaf nodes *)
    let rec init i =
      if i < pow2_size then (
        init (lchild i) ; init (rchild i) ;
        arr.(i) <- M.reduce (arr.(lchild i)) (arr.(rchild i))
      )
    in
    init 1 ; (pow2_size,arr)

  (* Traverse up tree, updating i and its ancestors *)
  let rec update_ancestors i arr =
    if i >= 1 then (
      arr.(i) <- M.reduce (arr.(lchild i)) (arr.(rchild i));
      update_ancestors (parent i) arr
    )

  let update_in_place f idx (size,arr) =
    arr.(idx+size) <- f (arr.(idx+size));
    update_ancestors (parent (idx+size)) arr

  let update idx newval (size,arr) =
    arr.(idx+size) <- newval;
    update_ancestors (parent (idx+size)) arr

  let total_sum (size,arr) = arr.(1)

  (* Sum over range [i,j) *)
  let interval_sum =
    (* Bottom up approach to sum range [i,j] (note inclusive here).
        To sum [i,j] at a level determined by offset, we sum all
        parents where both children are in the interval. This could
        leave out i and/or j, so we then add those in if needed. *)
    let rec isum' arr i j offset =
      if i > j then
        M.identity
      else if i = j then
        arr.(offset+i)
      else
        match (is_lchild i, is_lchild j) with
          | (true , false) -> isum' arr (i/2) (j/2) (offset/2)
          | (false, false) -> M.reduce (arr.(offset+i)) (isum' arr ((i+1)/2) (j/2) (offset/2))
          | (true , true ) -> M.reduce (arr.(offset+j)) (isum' arr (i/2) ((j-1)/2) (offset/2))
          | (false, true ) -> M.reduce (arr.(offset+i)) (M.reduce (arr.(offset+j))
                                (isum' arr ((i+1)/2) ((j-1)/2) (offset/2)) )
      in
    fun i j (size, arr) ->
      isum' arr i (j-1) size
end

module Immutable = Seglist (IntPlus)

(* Some testing! *)
let () =
  let size = 5 in
  let arr = Array.init size (fun _ -> 1) in
  let list = Immutable.make arr in
  print_endline(string_of_int (Immutable.interval_sum 0 5 list));
  let list' = Immutable.update 0 2 list in
  print_endline(string_of_int (Immutable.interval_sum 0 5 list'))