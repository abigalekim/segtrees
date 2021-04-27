/** @file segtree.cpp
 *  @author Danny Sleator, translated by Abi Kim (abigalek)
 *  @brief Segment tree implementation in C++. 
 *  Implements the segtree described in the lectures notes.
 */ 


#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

class Segtree {
    public:
        // Constructor for segtrees.
        Segtree(int size) {
            n = size;
            N = SuperCeiling(size);
            A.resize(2 * N, identity);
        }

        // Implements Assign in the lecture notes.
        // Assigns value x to index i in the array A, and also recomputes
        // every other value in the path up to the root from index i by 
        // adding together the values of the two children below it.
        void Assign(int i, int x) {
            i = i+N;
            A[i] = x;
            for (i = Parent(i); i>0; i = Parent(i)) {
                A[i] = glue (A[LeftChild(i)], A[RightChild(i)]);
            }
        }

        // Finds sum_{i <= k <= j} A[k]
        // More information in function f's documentation.
        int RangeSum(int i, int j) {
            return f (1, 0, (N-1), i, j);
        }

        // Prints the contents of the array A
        void printA() {
            for (int i=0; i < 2*N; i++) {
                cout << "A[" << i << "] = " << A[i] << endl;
            }
        }

    private:
        int n;
        int N;
        vector<int> A;
        int identity = 0;

        // Returns smallest power of 2 greater than equal to n
        int SuperCeiling(int n) {
            int p;
            for (p=1; p < n; p = p << 1);
            return p;
        }

        // Tree index functions
        int Parent(int i) {return i/2;}
        int LeftChild(int i) {return 2*i;}
        int RightChild(int i) {return 2*i+1;}

        // Function that glues associative operator on elements
        int glue(int a, int b) {
            return a+b;
        }

        int f (int v, int l, int r, int i, int j) {
        /*  We’re currently at A[v]. 1 <= v < 2*N.
            The range [l,r] is that of the current block, wrt user variables [0,n-1].
            The range [i,j] is the range of the query, wrt user variables [0,n-1].
            The size of the range [l,r] = r-l+1 is a power of 2.
            The range [l,r] contains the range [i,j].
            This function returns the answer to the query.
        */
            int t1, t2, m;
            if (l==i && r==j) {
                return A[v];
            } else {
                m = (l+r)/2; // split [l,r] into [l,m] [m+1,r]
                t1 = (i <= m)? f (LeftChild(v), l, m, i, (min(m,j))): identity;
                t2 = (j > m)? f (RightChild(v), (m+1), r, (max(i,(m+1))), j): identity;
                return glue (t1, t2);
            }
        }
};

int main() {
    Segtree s(7);
    s.Assign(3,7);
    s.Assign(4,1);

    s.printA();
    
    printf(" RangeSum(2,7) = %d\n", s.RangeSum(2,7));
    printf(" RangeSum(0,3) = %d\n", s.RangeSum(0,3));
    printf(" RangeSum(4,5) = %d\n", s.RangeSum(4,5));
    printf(" RangeSum(5,5) = %d\n", s.RangeSum(5,5));
    return 0;
}