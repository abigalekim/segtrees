(* Allows you to choose between a mutable segtree
 * and an immutable one :) *)
signature SEQUENCE = sig 
    type 'a t 

    val tabulate : int * (int -> 'a) -> 'a t 
    val sub : 'a t * int -> 'a 
    val update : 'a t * int * 'a -> 'a t 
end 

structure MutableSequence : SEQUENCE = struct 
    type 'a t = 'a Array.array 

    val tabulate = Array.tabulate 
    val sub = Array.sub 
    fun update (A, i, x) = A before Array.update (A, i, x) 
end 

structure ImmutableSequence : SEQUENCE = struct 
    type 'a t = 'a Vector.vector

    val tabulate = Vector.tabulate 
    val sub = Vector.sub 
    val update = Vector.update 
end 

signature MONOID = sig 
    (* Element type *)
    type t

    (* An associative function *)
    val combine : t * t -> t
    (* A left and right identity for 'combine' *)
    val identity : t 

    val toString : t -> string 
end 

signature SEGTREE = sig 
    structure Monoid : MONOID

    type t 

    (* Creates a segtree of the given size
     * with all elements being the identity *)
    val empty : int -> t 

    (* Updates at the given index.
     * If the underlying storage is mutable,
     * then you can disregard the return result *)
    val update : t * int * Monoid.t -> t 

    (* rangeSum (T, start, finish)
     * Finds the range sum of [start, finish]
     * Note that the range is inclusive! *)
    val rangeSum : t * int * int -> Monoid.t 

    (* (Inefficiently) constructs a segtree from an initial list *)
    val fromList : Monoid.t list -> t 

    (* Converts just the "user elements" to a string
     * formatted as a list *)
    val toString : t -> string 
end 

fun assert e msg = if e then () else raise Fail msg

functor MkSegtree(structure Monoid: MONOID 
                  structure Sequence: SEQUENCE) 
                  : SEGTREE where type Monoid.t = Monoid.t = 
struct 
    structure Monoid = Monoid

    type t = Monoid.t Sequence.t * int

    fun parent i = i div 2
    fun leftChild i = 2 * i 
    fun rightChild i = (2 * i) + 1

    fun nextPowerOfTwo n = 
    let 
        fun go x = 
            if x >= n 
                then x 
                else go (2 * x)
    in 
        go 1 
    end 

    fun empty n = 
    let 
        val () = assert (n > 0) ("Length too small: " ^ Int.toString n)
        val n = nextPowerOfTwo n
    in 
        (Sequence.tabulate (2 * n, fn _ => Monoid.identity), n)
    end 

    fun update ((T, n), i, x) = 
    let 
        val idx = i + n 
        val T = Sequence.update (T, idx, x)

        fun updateParents T = fn 
               0 => T 
            | idx => 
                let 
                    val left = Sequence.sub (T, leftChild idx)
                    val right = Sequence.sub (T, rightChild idx)
                    val v = Monoid.combine (left, right)
                in
                    updateParents (Sequence.update (T, idx, v)) (parent idx)
                end 
    in
        (updateParents T (parent idx), n)
    end 
    
    fun rangeSum ((T, n), start, finish) = 
    let 
        val () = 
            assert (start >= 0 andalso finish < n) 
                (String.concatWith " " 
                    ["Indices out of bounds:", 
                     Int.toString start, "to", Int.toString finish]
                )

        fun f (v, l, r, i, j) = 
            if l = i andalso r = j then Sequence.sub (T, v) 
            else let 
                val m = (l + r) div 2 
                val t1 = if i <= m 
                    then f (leftChild v, l, m, i, Int.min (m, j))
                    else Monoid.identity 

                val t2 = if j > m 
                    then f (rightChild v, m + 1, r, Int.max (i, m + 1), j)
                    else Monoid.identity 
            in 
                Monoid.combine (t1, t2)
            end 

    in 
        f (1, 0, n - 1, start, finish)
    end 

    fun fromList L = 
    let 
        val n = List.length L 
        val S = empty n 
    in
        List.foldli (fn (idx, elem, T) => update (T, idx, elem)) S L
    end 

    fun toString (T, n) = 
    let 
        val elems = List.map Monoid.toString (List.tabulate (n, fn i => Sequence.sub (T, n + i)))
    in
        String.concat ["[", String.concatWith ", " elems, "]"]
    end 
end 

structure IntMonoid = struct 
    type t = int 
    val combine = op+
    val identity = 0
    val toString = Int.toString
end 

structure IntSegtree = MkSegtree(structure Monoid = IntMonoid 
                                 structure Sequence = ImmutableSequence)

(* Tests for immutable 😋 *)
val s = IntSegtree.fromList [5, 0, ~1, 7, 1, 0, 12]

val () = assert (IntSegtree.rangeSum (s, 0, 6) = 5+1+7-1+12) "Whole range"
val () = assert (IntSegtree.rangeSum (s, 3, 3) = 7) "Single elem"
val () = assert (IntSegtree.rangeSum (s, 2, 4) = 7) "Smaller range"
val () = print (IntSegtree.toString s ^ "\n")

(* Tests for mutable 🤢 *)
structure IntSegtree = MkSegtree(structure Monoid = IntMonoid 
                                 structure Sequence = MutableSequence)

(* Tests *)
val s = IntSegtree.fromList [5, 0, ~1, 7, 1, 0, 12]

val () = assert (IntSegtree.rangeSum (s, 0, 6) = 5+1+7-1+12) "Whole range"
val () = assert (IntSegtree.rangeSum (s, 3, 3) = 7) "Single elem"
val () = assert (IntSegtree.rangeSum (s, 2, 4) = 7) "Smaller range"
val () = print (IntSegtree.toString s ^ "\n")
