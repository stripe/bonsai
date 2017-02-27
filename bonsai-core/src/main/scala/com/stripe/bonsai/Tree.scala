package com.stripe.bonsai

import scala.language.existentials

import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer

import com.stripe.bonsai.layout.Vec

/**
 * A succinct data structure for representing k-ary trees with arbitrary data
 * attached to each node.
 *
 * In general, you can expect that for reasonably large `Tree`s will require
 * ~2.73bits per node to store the *structure* of the tree. The structure of
 * the tree includes just the parent/child relationships.
 *
 * @param bitset the underlying bitset that supports fast rank/select
 * @param labels the label associated with each node, indexed by its label
 */
class Tree[A](val bitset: IndexedBitSet, val labels: Vec[A]) {
  import Tree.NodeRef

  private def mkNodeRef(index: Int): Option[NodeRef[A]] =
    if (bitset(index)) {
      Some(NodeRef(this, bitset.rank(index) - 1))
    } else {
      None
    }

  def root: Option[NodeRef[A]] =
    mkNodeRef(0)

  def isEmpty: Boolean =
    root.isEmpty

  override def toString: String = {
    def show(node: NodeRef[A]): String = {
      s"${node.label} -> ${node.children.map(show).toList}"
    }
    s"Tree(${root.map(show).getOrElse("")})"
  }

  def firstChild(node: NodeRef[A]): Option[NodeRef[A]] =
    mkNodeRef(Tree.firstChildIndex(node.index))

  def nextSibling(node: NodeRef[A]): Option[NodeRef[A]] =
    mkNodeRef(Tree.nextSiblingIndex(node.index))
}

object Tree {

  @inline final def firstChildIndex(index: Int) = 2 * index + 1
  @inline final def nextSiblingIndex(index: Int) = 2 * index + 2

  implicit def BonsaiTreeOps[A]: TreeOps[Tree[A], A] =
    new TreeOps[Tree[A], A] {
      type Node = NodeRef[A]

      def root(t: Tree[A]): Option[Node] = t.root
      def children(node: Node): Iterable[Node] = node
      def label(node: Node): A = node.label

      // optimized version of reduce
      // this method avoids allocating any extra Node instances.
      override def reduce[X](node: Node)(f: (A, Iterable[X]) => X): X = {
        val tree = node.tree
        def assemble(index: Int): Iterable[X] = {
          var buf = List.empty[X]
          var i = Tree.firstChildIndex(index)
          while (tree.bitset(i)) {
            val n = tree.bitset.rank(i) - 1
            buf = f(tree.labels(n), assemble(n)) :: buf
            i = Tree.nextSiblingIndex(n)
          }
          buf
        }
        f(node.label, assemble(node.index))
      }
    }

  /**
   * Returns an empty, 0-node bonsai `Tree`.
   */
  def empty[A: Layout]: Tree[A] =
    new Tree(IndexedBitSet.empty, Layout[A].empty)

  /**
   * Constructs a bonsai `Tree` from some other arbitrary tree structure. The 2
   * trees will be structurally equivalent, though the bonsai tree will (likely)
   * require much less space.
   *
   * @param tree the tree whose structure we are copying
   */
  def apply[T, L](tree: T)(implicit ev: TreeOps[T, L], ll: Layout[L]): Tree[L] = {
    import ev._

    // We accept k-ary trees, but actually need binary trees for the succinct
    // data structure. So, we use the first-child/sibling representation to
    // turn the k-ary tree into a binary tree. Basically, each node has at most
    // 2 pointers: a pointer to the node's first child, and a pointer to its
    // sibling (common parent only). This is enough to represent the tree in
    // its entirety; all we do is remove all the old edges and instead use the
    // pointers as the new edges in the tree. This new tree consists of nodes
    // with at most 2 edges each - hence binary!

    sealed trait BinaryNode

    // An internal nodes represents a real node (user visible) in the tree
    // we're building. We wrap both the node itself and its following siblings
    // (rightSpine) since we're transforming the underlying tree structure to
    // the new first-child/sibling representation. This let's us return the the
    // left/right child in the transformed tree in constant time.
    //
    // As an example, consider the following transform:
    //
    //       1               1
    //      /|\             /
    //     / | \   ---\    /
    //    2  3  4  ---/   2 -> 3 -> 4
    //   / \             /
    //  5   6           5 -> 6
    //
    // The root of the tree has no siblings, so its representation is simple:
    //
    //  - root: `InternalNode(1, Nil)`.
    //
    // We represent node 2 as `InternalNode(2, List(3, 4))`. We then can get:
    //
    //  - 2's left child: `InternalNode(5, List(6))`, and
    //  - 2's right child: `InternalNode(3, List(4))`.
    //
    case class InternalNode(node: Node, rightSpine: List[Node]) extends BinaryNode {
      def leftChild: BinaryNode = BinaryNode.fromSpine(node.children.toList)
      def rightChild: BinaryNode = BinaryNode.fromSpine(rightSpine)
    }

    // Part of the succinct data structure for trees is the notion of external
    // nodes. These are nodes that aren't actually part of the user-visible
    // tree, but are required for the invariants of the succinct data structure
    // to work correctly. The invariants are namely these:
    //
    //  - Internal nodes have exactly 2 children.
    //  - External nodes have no children.
    //
    // The external nodes are added to internal nodes with less than 2 children
    // so that they then have 2 children. External nodes are used in the
    // construction of the underlying bitset.
    case object ExternalNode extends BinaryNode

    object BinaryNode {
      def fromSpine(nodes: List[Node]): BinaryNode = nodes match {
        case node :: rightSpine => InternalNode(node, rightSpine)
        case Nil => ExternalNode
      }
    }

    val bitsBldr = IndexedBitSet.newBuilder
    val labelBldr = ll.newBuilder

    // We build the datastructure in this loop. We traverse the transformed
    // tree in level-order (breadth-first search). Each internal node is marked
    // in the bitset as 1 and each external node is marked as 0. Essentially,
    // what we are doing is labelling each internal node, starting from 1
    // (root), in level-order. The label for some internal node in the binary
    // tree can be retrieved using IndexedBitSet's `rank`. This let's avoid actually
    // storing each invidivual label.
    def build(nodes: Queue[BinaryNode]): Unit =
      if (nodes.nonEmpty) {
        nodes.dequeue match {
          case (inode @ InternalNode(node, _), rest) =>
            bitsBldr += true
            labelBldr += node.label
            build(rest.enqueue(inode.leftChild).enqueue(inode.rightChild))

          case (ExternalNode, rest) =>
            bitsBldr += false
            build(rest)
        }
      }

    tree.root match {
      case Some(node) =>
        // The root node has no siblings (no right child).
        build(Queue(InternalNode(node, Nil)))
        new Tree(bitsBldr.result(), labelBldr.result())

      case None =>
        Tree.empty
    }
  }

  /**
   * A reference to a node in a bonsai `Tree`.
   */
  final case class NodeRef[@specialized A] private[bonsai] (val tree: Tree[A], val index: Int) extends Iterable[NodeRef[A]] { node =>
    def label: A = tree.labels(index)

    // We extend Iterable largely to avoid allocating an extra object in
    // `children`.
    def iterator: Iterator[NodeRef[A]] =
      new Iterator[NodeRef[A]] {
        // TODO: Just use index directly.
        var child = tree.firstChild(node)
        def hasNext: Boolean = child.isDefined
        def next: NodeRef[A] = {
          val result = child.get
          child = tree.nextSibling(result)
          result
        }
      }

    def children: Iterable[NodeRef[A]] =
      this
  }
}
