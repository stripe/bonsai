package com.stripe.bonsai

import scala.language.existentials

import scala.collection.immutable.Queue

import com.stripe.bonsai.layout.Vec

class FullBinaryTree[A, B](
  val bitset: Bitset,
  val leaf: Bitset,
  val branchLabels: Vec[A],
  val leafLabels: Vec[B]
) { tree =>

  private def mkNodeRef(bitsetIndex: Int): NodeRef = {
    val index = bitset.rank(bitsetIndex) - 1
    if (tree.leaf(index)) new LeafRef(index) else new BranchRef(index)
  }

  def root: Option[NodeRef] = if (bitset(0)) Some(mkNodeRef(0)) else None

  def isEmpty: Boolean = bitset(0)

  sealed abstract class NodeRef {
    def fold[R](f: (NodeRef, NodeRef, A) => R, g: B => R): R
  }

  final class LeafRef private[FullBinaryTree] (index: Int) extends NodeRef {
    def label: B = leafLabels(bitset.rank(index) - 1)
    def fold[R](f: (NodeRef, NodeRef, A) => R, g: B => R): R = g(label)
  }

  final class BranchRef private[FullBinaryTree] (index: Int) extends NodeRef {
    def label: A = branchLabels(index - bitset.rank(index))
    def leftChild: NodeRef = tree.mkNodeRef(2 * index + 1)
    def rightChild: NodeRef = tree.mkNodeRef(2 * index + 2)
    def fold[R](f: (NodeRef, NodeRef, A) => R, g: B => R): R = f(leftChild, rightChild, label)
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
    new FullBinaryTree(Bitset.empty, Bitset.empty, Layout[A].empty, Layout[B].empty)

  /**
   * Constructs a bonsai `Tree` from some other arbitrary tree structure. The 2
   * trees will be structurally equivalent, though the bonsai tree will (likely)
   * require much less space.
   *
   * @param tree the tree whose structure we are copying
   */
  def apply[T, B, L](tree: T)(implicit ev: FullBinaryTreeOps[T, B, L]): FullBinaryTree[B, L] = {
    import ev._

    val bitsBldr = Bitset.newBuilder
    val leafBldr = Bitset.newBuilder
    val branchLabelBldr = Layout[B].newBuilder
    val leafLabelBldr = Layout[L].newBuilder

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
