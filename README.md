# Haskell Rose Trie

A rose tree is a tree of trees, where every node contains a leaf and set of
sub-trees. A trie is a tree structure where each node can be accessed using a
"path," where a path is a list of keys. A rose-trie does both. RoseTries are
best used in situations where every leaf in the Trie needs to accessible by a
list of polymorphic path elements.

The underlying implementation is based on "Data.Map", from the Haskell
Platform's "containers" package. So unlike the ordinary rose tree, where each
node contains a list of sub-trees, the RoseTrie contains a Map of sub-trees,
where each key in the Map forms a single link in the trie path. As a result, the
path for the RoseTrie is a list data type that is polymorphic over list elements
which instantiate both the 'Prelude.Ord' and 'Prelude.Eq' type classes.

Operating on a RoseTrie with a path of length `p` performs up to `p` times a
number of `O(log n)` Map operations on each of the sub-tree Map structures
containing `n` elements, therefore path lookups and insertions in a RoseTrie
data structure are on the order of `O(p * log n)` time complexity.

This library was originally part of the Dao package, but has been branched
into it's own package in the hopes that it will be useful in a wider
variety of projects.

