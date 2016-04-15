package com.stripe.bonsai

import java.io.IOException

import scala.language.existentials

import scala.collection.immutable.Queue

import java.io.{ DataInput, DataOutput }

import com.stripe.bonsai.layout.Vec

/**
 * This type represents a binary tree, where each branch is guaranteed
 * to have two children (i.e. it is "full").
 *
 * =Members=
 *
 * {{{
 *  - bitset: encodes the tree structure using a concept of internal
 *    and external nodes, e.g. a (potentially unbalanced) heap.  it's
 *    sort of complicated. basically, for non-balanced trees, we need
 *    to add "spacing" so that the heap addressing invariants are
 *    preserved.
 *
 *  - isLeaf: encodes whether an (internal) node is a leaf or branch
 *    node.
 *
 *  - branchLabels: a dense array of branch label values to index
 *    into. we use isLeaf to convert 'bitset indices' into
 *    'branchLabels' indices.
 *
 *  - leafLabels: a dense array of leaf label values to index into. we
 *    use isLeaf to convert 'bitset indices' into 'leafLabels' indices.
 * }}}
 *
 * =Examples=
 *
 * ==Example 1==
 *
 * {{{
 *       a
 *      / \
 *     b   c
 *    / \ / \
 *   d  e f  g
 *
 * (each leaf has two fake "external nodes" which are 0's in bitset)
 *
 * bitset: 111111100000000
 * isLeaf: 0001111
 * branchLabels: a,b,c
 * leafLabels: d,e,f,g
 * }}}
 *
 * ==Example 2==
 *
 * {{{
 *       a
 *      / \
 *     b   c
 *        / \
 *        d  e
 *
 * (each leaf has two fake "external nodes" which are 0's in bitset)
 *
 * bitset: 11100110000
 * isLeaf: 01011
 * branchLabels: a,c
 * leafLabels: b,d,e
 * }}}
 *
 */
class FullBinaryTree[A, B](
  val bitset: IndexedBitSet,
  val isLeaf: IndexedBitSet,
  val branchLabels: Vec[A],
  val leafLabels: Vec[B]
) { tree =>

  private def mkNodeRef(bitsetIndex: Int): NodeRef = {
    val index = bitset.rank(bitsetIndex) - 1
    if (tree.isLeaf(index)) {
      new LeafRef(index)
    } else {
      new BranchRef(index)
    }
  }

  final def reduce[X](f: (A, X, X) => X)(g: B => X): Option[X] =
    if (nonEmpty) Some(FullBinaryTree.reduceNode(this, 0)(f)(g)) else None

  final def root: Option[NodeRef] =
    if (nonEmpty) Some(mkNodeRef(0)) else None

  final def nonEmpty: Boolean = bitset(0)
  final def isEmpty: Boolean = !bitset(0)

  override def equals(that: Any): Boolean = that match {
    case (that: FullBinaryTree[_, _]) =>
      this.bitset == that.bitset &&
      this.isLeaf == that.isLeaf &&
      this.branchLabels == that.branchLabels &&
      this.leafLabels == that.leafLabels

    case _ =>
      false
  }

  override def hashCode: Int = {
    17 * (bitset.hashCode + (23 * isLeaf.hashCode +
      (47 * branchLabels.hashCode + (19 * leafLabels.hashCode))))
  }

  sealed abstract class NodeRef {
    def fold[R](f: (NodeRef, NodeRef, A) => R, g: B => R): R
    def reduce[X](f: (A, X, X) => X)(g: B => X): X
  }

  final def leafLabel(index: Int): B =
    leafLabels(isLeaf.rank(index) - 1)

  final def branchLabel(index: Int): A =
    branchLabels(index - isLeaf.rank(index))

  final class LeafRef private[FullBinaryTree] (index: Int) extends NodeRef {
    def label: B =
      tree.leafLabel(index)
    def fold[R](f: (NodeRef, NodeRef, A) => R, g: B => R): R =
      g(label)
    def reduce[X](f: (A, X, X) => X)(g: B => X): X =
      g(label)
  }

  final class BranchRef private[FullBinaryTree] (index: Int) extends NodeRef {
    def label: A =
      tree.branchLabel(index)
    def leftChild: NodeRef =
      tree.mkNodeRef(2 * index + 1)
    def rightChild: NodeRef =
      tree.mkNodeRef(2 * index + 2)
    def fold[R](f: (NodeRef, NodeRef, A) => R, g: B => R): R =
      f(leftChild, rightChild, label)
    def reduce[X](f: (A, X, X) => X)(g: B => X): X =
      FullBinaryTree.reduceNode(tree, index)(f)(g)
  }
}

object FullBinaryTree {
  implicit def BonsaiFullBinaryTreeOps[A, B]: FullBinaryTreeOps[FullBinaryTree[A, B], A, B] =
    new FullBinaryTreeOps[FullBinaryTree[A, B], A, B] {
      type Node = FullBinaryTree[A, B]#NodeRef

      def root(t: FullBinaryTree[A, B]): Option[Node] =
        t.root

      def foldNode[X](node: Node)(f: (Node, Node, A) => X, g: B => X): X =
        node.fold(f, g)

      override def reduce[X](node: Node)(f: (Label, Iterable[X]) => X): X =
        node.reduce[X]((a, x1, x2) => f(Left(a), x1 :: x2 :: Nil))(b => f(Right(b), Nil))
    }

  /**
   * Returns an empty, 0-node bonsai `Tree`.
   */
  def empty[A: Layout, B: Layout]: FullBinaryTree[A, B] =
    new FullBinaryTree(IndexedBitSet.empty, IndexedBitSet.empty, Layout[A].empty, Layout[B].empty)

  /**
   * Constructs a bonsai `Tree` from some other arbitrary tree structure. The 2
   * trees will be structurally equivalent, though the bonsai tree will (likely)
   * require much less space.
   *
   * @param tree the tree whose structure we are copying
   */
  def apply[T, B, L](tree: T)(implicit ev: FullBinaryTreeOps[T, B, L], lb: Layout[B], ll: Layout[L]): FullBinaryTree[B, L] = {
    import ev._

    val bitsBldr = IndexedBitSet.newBuilder
    val leafBldr = IndexedBitSet.newBuilder
    val branchLabelBldr = lb.newBuilder
    val leafLabelBldr = ll.newBuilder

    def build(nodes: Queue[Option[Node]]): Unit =
      if (nodes.nonEmpty) {
        nodes.dequeue match {
          case (None, rest) =>
            bitsBldr += false
            build(rest)
          case (Some(node), rest) =>
            bitsBldr += true
            val (ol, or) = foldNode(node)({ (lc, rc, bl) =>
              leafBldr += false
              branchLabelBldr += bl
              (Some(lc), Some(rc))
            }, { ll =>
              leafBldr += true
              leafLabelBldr += ll
              (None, None)
            })
            build(rest.enqueue(ol).enqueue(or))
        }
      }

    tree.root match {
      case Some(node) =>
        build(Queue(Some(node)))
        new FullBinaryTree(bitsBldr.result(), leafBldr.result(),
          branchLabelBldr.result(), leafLabelBldr.result())

      case None =>
        FullBinaryTree.empty
    }
  }

  final def reduceNode[A, B, X](tree: FullBinaryTree[A, B], index: Int)(f: (A, X, X) => X)(g: B => X): X =
    if (tree.isLeaf(index)) {
      g(tree.leafLabel(index))
    } else {
      val label = tree.branchLabel(index)
      val x1 = reduceNode(tree, tree.bitset.rank(2 * index + 1) - 1)(f)(g)
      val x2 = reduceNode(tree, tree.bitset.rank(2 * index + 2) - 1)(f)(g)
      f(label, x1, x2)
    }

  val MagicNum = 0x66797883657302L // BONSAI/2

  def write[A: Layout, B: Layout](tree: FullBinaryTree[A, B], out: DataOutput): Unit = {
    out.writeLong(MagicNum)
    Layout[A].write(tree.branchLabels, out)
    Layout[B].write(tree.leafLabels, out)
    IndexedBitSet.write(out, tree.isLeaf)
    out.writeInt(tree.bitset.length)
    IndexedBitSet.write(out, tree.bitset)
  }

  def read[A: Layout, B: Layout](in: DataInput): FullBinaryTree[A, B] = {
    if (in.readLong() != MagicNum) {
      throw new IOException("not a Bonsai tree: no magic number")
    }

    val branchLabels = Layout[A].read(in)
    val leafLabels = Layout[B].read(in)
    val isLeaf = IndexedBitSet.read(in, branchLabels.size + leafLabels.size)
    val bitsetLength = in.readInt()
    val bitset = IndexedBitSet.read(in, bitsetLength)
    new FullBinaryTree(bitset, isLeaf, branchLabels, leafLabels)
  }
}
