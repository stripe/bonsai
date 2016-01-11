package com.stripe.bonsai

import scala.language.existentials

import scala.collection.immutable.Queue

import com.stripe.bonsai.layout.Vec

/**
 * This type represents a binary tree, where each branch is guaranteed
 * to have two children (i.e. it is "full").
 *
 * Members:
 *
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
 *
 * Example 1:
 *
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
 *
 * Example 2:
 *
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

  final def root: Option[NodeRef] =
    if (bitset(0)) Some(mkNodeRef(0)) else None

  final def isEmpty: Boolean =
    bitset(0)

  sealed abstract class NodeRef {
    def fold[R](f: (NodeRef, NodeRef, A) => R, g: B => R): R
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
}
