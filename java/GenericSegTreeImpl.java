/** @file GenericSegTreeImpl.java
 *  @author Danny Sleator, translated by Abi Kim (abigalek)
 *  @brief Generic segment tree implementation in Java.
 *  Implements the segtree described in the lectures notes.
 */ 

import java.io.*;
import java.util.function.BinaryOperator;
import java.util.*;

class SegTree<T> {    
    private int n;
    private int N;
    ArrayList<T> A;
    private T identity;
    BinaryOperator<T> glue;

    // Returns smallest power of 2 greater than equal to n
    private static int SuperCeiling(int n) {
        int p;
        for (p = 1; p < n; p = 2 * p);
        return p;
    }

    // Tree index functions
    private static int Parent(int i) {return i/2;}
    private static int LeftChild(int i) {return 2*i;}
    private static int RightChild(int i) {return 2*i+1;}

    private T f (int v, int l, int r, int i, int j) {
    /*  We’re currently at A[v]. 1 <= v < 2*N.
        The range [l,r] is that of the current block, wrt user variables [0,n-1].
        The range [i,j] is the range of the query, wrt user variables [0,n-1].
        The size of the range [l,r] = r-l+1 is a power of 2.
        The range [l,r] contains the range [i,j].
        This function returns the answer to the query.
    */
        int m;
        T t1, t2;
        if (l==i && r==j) {
            return A.get(v);
        } else {
            m = (l+r)/2; // split [l,r] into [l,m] [m+1,r]
            t1 = (i <= m) ? f (LeftChild(v), l, m, i, (Math.min(m,j))): identity;
            t2 = (j > m) ? f (RightChild(v), (m+1), r, (Math.max(i,(m+1))), j): identity;
            return glue.apply (t1, t2);
        }
    }

    // Constructor for segtrees.
    public SegTree(int size, T i, BinaryOperator<T> glue_fn) {
        n = size;
        N = SuperCeiling(size);
        A = new ArrayList<>();
        for (int j = 0; j < 2 * N; j++) {
            A.add(i);
        }
        identity = i;
        glue = glue_fn;
    }

    // Implements Assign in the lecture notes.
    // Assigns value x to index i in the array A, and also recomputes
    // every other value in the path up to the root from index i by 
    // adding together the values of the two children below it.
    public void Assign(int i, T x) {
        i = i + N;
        A.set(i, x);
        for (i = Parent(i); i > 0; i = Parent(i)) {
            A.set(i, glue.apply(A.get(LeftChild(i)), A.get(RightChild(i))));
        }
    }

    // Finds sum_{i <= k <= j} A[k]
    // More information in function f's documentation.
    public T RangeSum(int i, int j) {
        return f(1, 0, (N-1), i, j);
    }

    // Prints the contents of the array A
    public void printA() {
        for (int i=0; i < 2*N; i++) {
            System.out.print("A[" + String.valueOf(i) + "] = ");
            System.out.println(A.get(i));
        }
    }

};

public class GenericSegTreeImpl {
    public static void main(String[] args) {
        SegTree<Integer> s = new SegTree<Integer>(7, 0, Integer::sum);
        s.Assign(3,7);
        s.Assign(4,1);

        s.printA();

        System.out.println(" RangeSum(2,7) = " + String.valueOf(s.RangeSum(2,7)));
        System.out.println(" RangeSum(0,3) = " + String.valueOf(s.RangeSum(0,3)));
        System.out.println(" RangeSum(4,5) = " + String.valueOf(s.RangeSum(4,5)));
        System.out.println(" RangeSum(5,5) = " + String.valueOf(s.RangeSum(5,5)));
  }
}