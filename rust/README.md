To use this code for 451 (i.e., with rustc) do the following:

   1. Copy the files into your working directory.
   1. Rename `lib.rs` to `segtree.rs`
   1. Your file with main should include the line

   ```rust
   mod segtree;
   ```

Then you can use the library by doing `segtree::SegmentTree::new`,
etc...
