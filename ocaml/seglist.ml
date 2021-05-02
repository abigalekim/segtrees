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

module IntMin = struct
  type t = int
  let reduce x y = min x y
  let identity = Int.max_int
end

module Seglist (M : Monoid)
 = struct
  type elem = M.t
  type tree = Leaf of elem | Node of {left : tree; right : tree; acc : elem; size : int}

  let tree_sum = function
  | Leaf x -> x
  | Node{acc} -> acc

  let tree_size = function
  | Leaf _ -> 1
  | Node{size} -> size

  type t = Empty | Tree of tree

  let size = function
  | Empty -> 0
  | Tree t -> tree_size t

  let make elems =
    if (Array.length elems) = 0 then Empty else
    let rec build i j =
      if i+1 = j then Leaf (elems.(i)) else
      let mid = (i+j)/2 in
      let left = build i mid in
      let right = build mid j in
      Node {left; right; size = j-i; acc = M.reduce (tree_sum left) (tree_sum right)}
      in
    Tree (build 0 (Array.length elems))

  let update =
    let rec update' i newval = fun tree ->
      assert (0 <= i && i < tree_size tree) ;
      match tree with
      | Leaf _ -> Leaf newval
      | Node{left;right;size;acc} ->
          let left_size = tree_size left in
          if i < left_size then
            Node{left = update' i newval left; right; size; acc}
          else
            Node{left; right = update' (i-left_size) newval right; size; acc}
      in
    fun list i newval ->
    match list with
    | Empty -> raise (Failure "should be unreachable")
    | Tree tree -> Tree (update' i newval tree)

  (* Sum over range [i,j) *)
  let interval_sum =
    let rec isum' i j = function
      | Leaf x -> x (* i=0, j=1 *)
      | Node {left; right; size; acc} ->
          if i = 0 && j = size then acc else
          let left_size = tree_size left in
          if j <= left_size then
            isum' i j left
          else if 0 <= i-left_size then
            isum' (i-left_size) (j-left_size) right
          else
            M.reduce (isum' i left_size left) (isum' 0 (j-left_size) right)
      in
    
    fun i j -> assert (0 <= i  && i < j); function
    | Empty -> assert (j = 1);
        M.identity
    | Tree tree -> assert (j <= tree_size tree);
        isum' i j tree
end

module SeglistMutable (M : Monoid)
 = struct
  type elem = M.t
  type t = int * elem array

  let lchild i = 2*i
  let rchild i = 2*i+1
  let parent i = i/2
  let is_lchild i = (i mod 2 = 0)

  let make elems =
    let pow2_size = 1 lsl let rec log x acc =
                            if x <= 1 then acc else log (x/2) (acc+1)
                          in log (Array.length elems - 1) 1
      in
    assert (Array.length elems <= pow2_size) ;
    let end_elems = pow2_size + Array.length elems in
    let arr = Array.init (2*pow2_size)
      (fun i -> if i < pow2_size || i >= end_elems then M.identity else elems.(i-pow2_size))
      in
    let rec init i =
      if i < pow2_size then (
        init (lchild i) ; init (rchild i) ;
        arr.(i) <- M.reduce (arr.(lchild i)) (arr.(rchild i))
      )
    in
    init 1 ; (pow2_size,arr)

  let update =
    let rec update' i arr =
      if i >= 1 then
        arr.(i) <- M.reduce (arr.(lchild i)) (arr.(rchild i));
        update' (parent i) arr
    in
    fun idx newval (size,arr) ->
      arr.(idx+size) <- newval;
      update' (parent (idx+size)) arr

  (* Sum over range [i,j) *)
  let interval_sum =
    (* sums over range [i,j] (note inclusive now) *)
    let rec isum' arr i j offset =
      assert (0 <= i);
      assert (i <= j+1);
      assert (j <= offset);
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
      assert (0 <= i  && i < j && j < size);
      isum' arr i (j-1) size
end

module Immutable = Seglist (IntPlus)
module Mutable = SeglistMutable (IntPlus)

(* Some testing! *)
let () =
  let size = 4000 in
  let arr = Array.init size (fun _ -> 1) in
  print_endline ("ready...") ;
  let list = Mutable.make arr in
  print_endline ("start!") ;
  for i = 0 to size-1 do
    for j = i+1 to size do
      let _ = Mutable.interval_sum i j list in ()
    done
  done