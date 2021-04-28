//! Translated from code by Danny Sleator. In the public domain.

use std::ops::RangeBounds;

pub mod generic;

pub struct SegmentTree(generic::SegmentTree<i32, fn(i32, i32) -> i32>);

impl SegmentTree {
    pub fn new(size: usize) -> Self {
        Self(generic::SegmentTree::new(size, std::ops::Add::add, 0))
    }

    pub fn assign(&mut self, i: usize, x: i32) {
        self.0.assign(i, x)
    }

    pub fn range_sum(&self, range: impl RangeBounds<usize>) -> i32 {
        self.0.range_sum(range)
    }
}
