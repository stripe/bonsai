# Bonsai

Beautiful trees, without the landscaping. Bonsai is a Scala library for
transforming arbitrary tree structures into read-only versions that take up a
fraction of the space.

## Overview

Bonsai compresses trees in 2 ways: by using significantly less space to store
the tree structure itself (tree compression), and by encoding the node labels
in a memory efficient structure (label compression).

### What is a "Tree"?

Bonsai works over arbitrary trees, so it assumes a fairly generic interface
for interacting with trees. In Bonsai a tree;

 * has 0 or 1 root nodes
 * each node has 0 or more children
 * each node has a label attached to it

The actual type of the node is unimportant. What is important is the node labels
and the relationships between the nodes (parent, child, sibling, etc). This
structure is enough to describe most of the types of trees you are familiar
with.

Bonsai encodes this notion of trees with the [TreeOps type class][treeops]. Here
is a truncated version of the type class:

```scala
trait TreeOps[Tree, Label] {

  /** The type of the nodes in the tree. */
  type Node

  /**
   * Returns the root node of the tree.
   */
  def root(t: Tree): Option[Node]

  /**
   * Returns all the direct children of the given node. The order may or may
   * not matter. TreeOps does not provide any guarantees here.
   */
  def children(node: Node): Iterable[Node]

  /**
   * Returns the label attached to the given node.
   */
  def label(node: Node): Label

  ...
}
```

The type `T` is our actual tree type. The `Node` type is the way we reference
internal nodes in our tree `T`. The actual type of `Node` isn't important,
however, and is mostly an implementation detail. The important bit is the
`Label` type, which is the user-facing data associated with each node.

The `bonsai-example` subproject has [an example of a Huffman tree][huffman].
A Huffman tree is used to store a Huffman coding for decoding a compressed
message (a bitstring). We decode the bitstring, bit-by-bit, using the tree.

Starting at the root of the tree, we follow the left child if the current bit
is a 0 and the right child if it is a 1. We continue until reaching a leaf node,
at which poitn we output the symbol associated with it, then start back at the
beginning of the tree.  When we've exhausted the entire bitstring, we'll have
our decoded message.

Here is how we may implement a Huffman tree in Scala:

```scala
sealed trait HuffmanTree[+A]
case class Branch[+A](zero: HuffmanTree[A], one: HuffmanTree[A]) extends HuffmanTree[A]
case class Leaf[+A](value: A) extends HuffmanTree[A]
```

And here is how we would implement its `TreeOps` instance:

```scala
import com.stripe.bonsai.TreeOps

object HuffmanTree {
  implicit def huffmanTreeOps[A]: TreeOps[HuffmanTree[A], Option[A]] =
    new TreeOps[HuffmanTree[A], Option[A]] {
      type Node = HuffmanTree[A]

      def root(tree: HuffmanTree[A]): Option[HuffmanTree[A]] = Some(tree)
      def children(tree: HuffmanTree[A]): Iterable[HuffmanTree[A]] = tree match {
        case Branch(l, r) => l :: r :: Nil
        case _ => Nil
      }
      def label(tree: HuffmanTree[A]): Option[A] = tree match {
        case Leaf(value) => Some(value)
        case _ => None
      }
    }
}
```

As long as we are careful to implement all our operations on a Huffman tree by
using its more generic `TreeOps` interface, rather than `HuffmanTree` directly,
we can then swap out the actual tree data structure, without affecting the code
using it.

For example, below we implement a `decode` operation as an implicit class using just `TreeOps`.

```scala
import scala.collection.immutable.BitSet

implicit class HuffmanTreeOps[T, A](tree: T)(implicit treeOps: TreeOps[T, Option[A]]) {
  // Importing treeOps gives us some useful methods on `tree`
  import treeOps._

  def decode(bits: BitSet, len: Int): Vector[A] = {
    val root = tree.root.get
    val (_, result) = (0 until len)
      .foldLeft((root, Vector.empty[A])) { case ((node, acc), i) =>
        node.label match {
          case Some(value) => (root, acc :+ value)
          case None if bits(i) => (node.children.head, acc)
          case None => (node.children.iterator.drop(1).next, acc)
        }
      }
    result
  }
}
```

The goal of this indirection through `TreeOps` is to let us use a compressed
version of the `tree` instead of an actual `HuffmanTree`, which will see below.

### Tree Compression

Bonsai's tree compression is based off of a [succinct data structure][succinct]
for binary trees. Bonsai supports k-ary trees by first transforming the
original tree into a [left-child right-sibling tree][k-ary-transform], which
preserves all the relationships from the original tree, but ensures we have at
most 2 children per node. You can read more about the details of the actual
compression algorithm used in
["Practical Implementation of Rank and Select Queries"][ggmn05]. **The upshot is
that we can store the entire structure of a tree in only ~2.73bits per node.**
This replaces the normal strategy of using JVM objects for nodes and references
to store the relationships.

We actually compress trees by transforming them into [Bonsai `Tree`s][tree].
Bonsai's `Tree` constructor takes any arbitrary tree `T` that has a
`TreeOps[T]` available and will return a [`Tree`][tree] with the same structure
and labels (and `Label` type) as the original tree. However, the entire
structure and labels of the tree will have been compressed, so this new tree
requires significantly less space.

In the example in `bonsai-example`, we use the Huffman encoding described above
to construct a simple Huffman tree for the printable ASCII characters (0x20 ->
0x7E) and compress it using Bonsai's `Tree`. The result is a **11x reduction**
in memory requirements. Since our `decode` operation was implemented using
`TreeOps`, we can use this compressed tree just as we would've used the
original tree.

This example is a bit contrived, since the trees are small to begin with, but
you can imagine that applying this to a large random forest yields great
results.

### Label Compression

Bonsai provides a [Layout][layout] type class, along with some simple
combinators, for describing how to (de)serialize your labels. At the lowest
level are a set of Layout "primitives" that can encode simple data types into
compact data structures. The combinators then allow more complex structures to
be described (tuples, `Either`, mappings to case classes, etc), without adding
much, if any, overhead.

Here is an example of a `Layout` for some `Widget` type we made up:

```scala
import com.stripe.bonsai.Layout

sealed trait Widget
case class Sprocket(radius: Int, weight: Option[Double]) extends Widget
case class Doodad(length: Int, width: Int, weight: Option[Double]) extends Widget

object Widget {
  implicit val WidgetLayout: Layout[Widget] = {
    Layout[Either[(Int, Option[Double]), ((Int, Int), Option[Double])]].transform(
      {
        case Left((r, wt)) => Sprocket(r, wt)
        case Right(((l, w), wt)) => Doodad(l, w, wt)
      },
      {
        case Sprocket(r, wt) => Left((r, wt))
        case Doodad(l, w, wt) => Right(((l, w), wt))
      }
    )
  }
}
```

You can see the [full Widget code/example][widget] in the `bonsai-example` sub
project. In that example, we compress a `Vector[Option[Widget]]` using the
layout and end up with over a **6x reduction** in memory requirements.

Currently, Bonsai focuses mainly on compressing the overhead of the structure
your data requires (eg options, eithers, tuples), rather than the data itself.
This will likely change in future releases, and we'll support better
compression for primitive types, as well as things like dictionary encoding for
all types.

# Using Bonsai in SBT or Maven

Bonsai is published on sonatype. To use it in your SBT project, you can add the
following to your `build.sbt`:

```scala
libraryDependencies += "com.stripe" %% "bonsai" % "0.3.0"
```

# Miscellaneous

Bonsai is Open Source and available under the MIT License.

For more help, feel free to contact the authors or create an issue.

[succinct]: https://en.wikipedia.org/wiki/Succinct_data_structure "Succinct Data Structures"
[ggmn05]: http://www.dcc.uchile.cl/~gnavarro/algoritmos/ps/wea05.pdf
[k-ary-transform]: https://en.wikipedia.org/wiki/Left-child_right-sibling_binary_tree
[layout]: https://github.com/stripe/bonsai/blob/master/bonsai-core/src/main/scala/com/stripe/bonsai/Layout.scala
[widget]: https://github.com/stripe/bonsai/blob/master/bonsai-example/src/main/scala/com/stripe/bonsai/example/Widget.scala
[treeops]: https://github.com/stripe/bonsai/blob/master/bonsai-core/src/main/scala/com/stripe/bonsai/TreeOps.scala
[tree]: https://github.com/stripe/bonsai/blob/master/bonsai-core/src/main/scala/com/stripe/bonsai/Tree.scala
[huffman]: https://github.com/stripe/bonsai/blob/master/bonsai-example/src/main/scala/com/stripe/bonsai/example/Huffman.scala
