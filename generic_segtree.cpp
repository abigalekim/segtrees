/** @file generic_segtree.cpp
 *  @author Danny Sleator, translated by Abi Kim (abigalek)
 *  @brief Segment tree implementation in C++. 
 *  This implementation supports generics for the data being
 *  stored in the segtreeusing templates.
 *  Implements the segtree described in the lectures notes.
 */ 


#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>

using namespace std;

template<typename ValueType>
class Segtree {
    public:
        // Constructor for segtrees.
        Segtree(size_t size, std::function<ValueType (ValueType, ValueType)> glue) : n(size), glue_func(glue)
        {
            N = SuperCeiling(size);
            A.resize(2 * N, identity);
        }

        // Implements Assign in the lecture notes.
        // Assigns value x to index i in the array A, and also recomputes
        // every other value in the path up to the root from index i by 
        // adding together the values of the two children below it.
        void Assign(size_t i, ValueType x) {
            i = i+N;
            A[i] = x;
            for (i = Parent(i); i>0UL; i = Parent(i)) {
                A[i] = glue_func (A[LeftChild(i)], A[RightChild(i)]);
            }
        }

        // Finds sum_{i <= k <= j} A[k]
        // More information in function f's documentation.
        ValueType RangeSum(size_t i, size_t j) {
            return f (1UL, 0UL, (N-1), i, j);
        }

        // Prints the contents of the array A
        void printA() {
            for (size_t i=0; i < 2*N; i++) {
                cout << "A[" << i << "] = " << A[i] << endl;
            }
        }

    private:
        size_t n;
        size_t N;
        vector<ValueType> A;
        ValueType identity = 0;
        std::function<ValueType (ValueType, ValueType)> glue_func;


        // Returns smallest power of 2 greater than equal to n
        size_t SuperCeiling(size_t n) {
            size_t p;
            for (p=1; p < n; p = p << 1);
            return p;
        }

        // Tree index functions
        size_t Parent(size_t i) {return i/2;}
        size_t LeftChild(size_t i) {return 2*i;}
        size_t RightChild(size_t i) {return 2*i+1;}

        ValueType f (size_t v, size_t l, size_t r, size_t i, size_t j) {
        /*  We’re currently at A[v]. 1 <= v < 2*N.
            The range [l,r] is that of the current block, wrt user variables [0,n-1].
            The range [i,j] is the range of the query, wrt user variables [0,n-1].
            The size of the range [l,r] = r-l+1 is a power of 2.
            The range [l,r] contains the range [i,j].
            This function returns the answer to the query.
        */
            ValueType t1, t2;
            size_t m;
            if (l==i && r==j) {
                return A[v];
            } else {
                m = (l+r)/2; // split [l,r] into [l,m] [m+1,r]
                t1 = (i <= m)? f (LeftChild(v), l, m, i, (min(m,j))): identity;
                t2 = (j > m)? f (RightChild(v), (m+1), r, (max(i,(m+1))), j): identity;
                return glue_func (t1, t2);
            }
        }
};

int main() {
    Segtree<int> s(7, plus<int>());
    s.Assign(3,7);
    s.Assign(4,1);

    s.printA();
    
    printf(" RangeSum(2,7) = %d\n", s.RangeSum(2,7));
    printf(" RangeSum(0,3) = %d\n", s.RangeSum(0,3));
    printf(" RangeSum(4,5) = %d\n", s.RangeSum(4,5));
    printf(" RangeSum(5,5) = %d\n", s.RangeSum(5,5));
    return 0;
}