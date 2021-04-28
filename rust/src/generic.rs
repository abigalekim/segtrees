use std::ops::{Bound, RangeBounds};

#[derive(Debug)]
pub struct SegmentTree<T, F> {
    n: usize,
    arr: Vec<T>,
    identity: T,
    glue: F,
}

impl<T, F> SegmentTree<T, F>
where
    T: Copy,
    F: Fn(T, T) -> T,
{
    // Call new from this module to construct a segment tree.
    pub fn new(size: usize, glue: F, identity: T) -> Self {
        let n = size.next_power_of_two();
        Self {
            n,
            arr: vec![identity; 2 * n],
            identity,
            glue,
        }
    }

    // Implements Assign in the lecture notes.
    // Assigns value x to index i in the array A, and also recomputes
    // every other value in the path up to the root from index i by
    // adding together the values of the two children below it.
    pub fn assign(&mut self, mut i: usize, x: T) {
        i += self.n;
        self.arr[i] = x;
        i = Self::parent(i);
        while i > 0 {
            self.arr[i] = (self.glue)(
                self.arr[Self::left_child(i)],
                self.arr[Self::right_child(i)],
            );
            i = Self::parent(i);
        }
    }

    // Finds sum_{i <= k <= j} A[k]
    // More information in function f's documentation.
    pub fn range_sum(&self, range: impl RangeBounds<usize>) -> T {
        let i = match range.start_bound() {
            Bound::Included(&i) => i,
            Bound::Excluded(&i) => i + 1,
            Bound::Unbounded => 0,
        };
        let j = match range.end_bound() {
            Bound::Included(&j) => j,
            Bound::Excluded(&j) => j - 1,
            Bound::Unbounded => self.n - 1,
        };

        self.f(1, 0, self.n - 1, i, j)
    }

    fn parent(i: usize) -> usize {
        i / 2
    }

    fn left_child(i: usize) -> usize {
        2 * i
    }

    fn right_child(i: usize) -> usize {
        (2 * i) + 1
    }

    fn f(&self, v: usize, l: usize, r: usize, i: usize, j: usize) -> T {
        // We're currently at A[v]. 1 <= v < 2 * N.
        // The range [l, r] is that of the current block, wrt user variables [0, n-1].
        // The range [i, j] is the range of the query, wrt user variables [0, n-1].
        // The size of the range [l, r] = r - l + 1 is a power of 2.
        // The range [l, r] contains the range [i, j].
        // This function returns the answer to the query.
        debug_assert!(i < self.arr.len() && j < self.arr.len());

        if l == i && r == j {
            self.arr[v]
        } else {
            let m = (l + r) / 2; // split [l, r] into [l, m] [m + 1, r]
            let t1 = if i <= m {
                self.f(Self::left_child(v), l, m, i, std::cmp::min(m, j))
            } else {
                self.identity
            };
            let t2 = if j > m {
                self.f(Self::right_child(v), m + 1, r, std::cmp::max(i, m + 1), j)
            } else {
                self.identity
            };
            (self.glue)(t1, t2)
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_add() {
        let mut s = SegmentTree::new(7, std::ops::Add::add, 0);
        s.assign(3, 7);
        s.assign(4, 1);

        assert_eq!(s.range_sum(2..=7), 8);
        assert_eq!(s.range_sum(0..=3), 7);
        assert_eq!(s.range_sum(4..=5), 1);
        assert_eq!(s.range_sum(5..=5), 0);
    }
}
