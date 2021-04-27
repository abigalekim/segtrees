/** @file segtree.cpp
 *  @author Danny Sleator, slightly modified by Abi Kim (abigalek)
 *  @brief Segment tree implementation in C.
 *  Now includes struct initialization support :)
 *  Unfortunately C doesn't have actual modules and the only way to
 *  sort of emulate modules is through different files, but we don't
 *  really have that liberty in 15-451, so this will have to do.
 *  Implements the segtree described in the lectures notes.
 */ 

#include <stdio.h>
#include <stdlib.h>

/**** HELPER FUNCTIONS ****/
// Returns the smallest power of 2 greater than or equal to n
int SuperCeiling(int n) {
    int p;
    for (p=1; p < n; p = p << 1);
    return p;
}

// Maximum/minimum functions
int max (int a, int b) {return (a>b)? a: b;}
int min (int a, int b) {return (a<b)? a: b;}

// Segment tree specific helpers
int Parent(int i) {return i/2;}
int LeftChild(int i) {return 2*i;}
int RightChild(int i) {return 2*i+1;}

// An arbitrary associative operator on elements
int glue(int a, int b) {
    return a+b;
}

/**** STRUCT DEFINITION ****/
typedef struct SegTree {
    int identity;
    int n;
    int N;
    int *A;
} SegTree;

/******************** INTERFACE FUNCTIONS BEGIN ********************/

void SegTreeInit(SegTree *s, int n) {
    s->n = n;
    s->N = SuperCeiling(n);
    s->A = (int *) malloc (sizeof(int) * (2*s->N));
    for (int i = 0; i < 2* s->N; i++) s->A[i] = 0;
}

void SegTreeDestroy(SegTree *s) {
    free(s->A);
}

// Implements Assign in the lecture notes.
// Assigns value x to index i in the array A, and also recomputes
// every other value in the path up to the root from index i by 
// adding together the values of the two children below it.
void Assign(SegTree *s, int i, int x) {
    i = i+s->N;
    s->A[i] = x;
    for (i = Parent(i); i>0; i = Parent(i)) {
        s->A[i] = glue (s->A[LeftChild(i)], s->A[RightChild(i)]);
    }
}

void printA(SegTree *s) {
    for (int i = 0; i < 2 * s->N; i++) {
        printf("A[%d] = %d\n", i, s->A[i]);
    }
}

int f (SegTree *s, int v, int l, int r, int i, int j) {
/* We’re currently at A[v]. 1 <= v < 2*N.
    The range [l,r] is that of the current block, wrt user variables [0,n-1].
    The range [i,j] is the range of the query, wrt user variables [0,n-1].
    The size of the range [l,r] = r-l+1 is a power of 2.
    The range [l,r] contains the range [i,j].
    This function returns the answer to the query.
*/
    int t1, t2, m;
    if (l==i && r==j) {
        return s->A[v];
    } else {
        m = (l+r)/2; /* split [l,r] into [l,m] [m+1,r] */
        t1 = (i <= m)? f (s, LeftChild(v), l, m, i, (min(m,j))): s->identity;
        t2 = (j > m)? f (s, RightChild(v), (m+1), r, (max(i,(m+1))), j): s->identity;
        return glue (t1, t2);
    }
}

// Finds sum_{i <= k <= j} A[k]
// More information in function f's documentation.
int RangeSum(SegTree *s, int i, int j) {
    return f (s, 1, 0, (s->N-1), i, j);
}

/******************** INTERFACE FUNCTIONS END ********************/

int main(){
    int i;
    SegTree s;
    SegTreeInit(&s, 7);
    Assign(&s, 3, 7);
    Assign(&s, 4, 1);

    printA(&s);
    
    printf(" RangeSum(2,7) = %d\n", RangeSum(&s, 2, 7));
    printf(" RangeSum(0,3) = %d\n", RangeSum(&s, 0, 3));
    printf(" RangeSum(4,5) = %d\n", RangeSum(&s, 4, 5));
    printf(" RangeSum(5,5) = %d\n", RangeSum(&s, 5, 5));

    SegTreeDestroy(&s);
}