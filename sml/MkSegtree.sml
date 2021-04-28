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
end 

signature SEGTREE = sig 
    structure Monoid : MONOID

    type t 

    (* Creates a segtree of the given size
     * with all elements being the identity *)
    val empty : int -> t 

    val update : t * int * Monoid.t -> t 
    val rangeSum : int * int -> Monoid.t 
end 

functor MkSegtree(structure Monoid: MONOID 
                  structure Sequence: SEQUENCE) 
                  : SEGTREE where type Monoid.t = Monoid.t = 
struct 
    structure Monoid = Monoid

    type t = Monoid.t Sequence.t * int

    fun parent i = i div 2
    fun leftChild i = 2 * i 
    fun rightChild i = (2 * i) + 1

    fun empty n = (n, Sequence.tabulate (2*n, fn _ => Monoid.identity))
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
                    updateParents (Sequence.update (T, idx, v) (parent idx)
                end 
        in
            (n, updateParents (parent idx))
        end 
    
    fun rangeSum _ = raise Fail "Unimplemented"
end 

structure IntMonoid = struct 
    type t = int 
    val combine = op+
    val identity = 0
end 

structure IntSegtree = MkSegtree(structure Monoid = IntMonoid 
                                 structure Sequence = ImmutableSequence)

