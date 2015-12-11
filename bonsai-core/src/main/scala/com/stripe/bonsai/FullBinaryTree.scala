package com.stripe.bonsai

import scala.language.existentials

import scala.collection.immutable.Queue

import com.stripe.bonsai.layout.Vec

class FullBinaryTree[A, B](
  val bitset: Bitset,
  val leaf: Bitset,
  val nodeLabels: Vec[A],
  val leafLabels: Vec[B]
) { tree =>
  private def mkNodeRef(bitsetIndex: Int): NodeRef =
    new NodeRef(bitset.rank(bitsetIndex) - 1)

  def root: Option[NodeRef] =
    if (bitset(0)) Some(mkNodeRef(0))
    else None

  def isEmpty: Boolean =
    root.isEmpty

  final class NodeRef private[FullBinaryTree] (index: Int) {
    def leftChild: NodeRef = tree.mkNodeRef(2 * index + 1)
    def rightChild: NodeRef = tree.mkNodeRef(2 * index + 2)
    def isLeaf: Boolean = tree.leaf(index)
    def nodeLabel: A = nodeLabels(index - bitset.rank(index))
    def leafLabel: B = leafLabels(bitset.rank(index) - 1)
  }
}

object FullBinaryTree {
  implicit def BonsaiFullBinaryTreeOps[A, B]: FullBinaryTreeOps[FullBinaryTree[A, B]] =
    new FullBinaryTreeOps[FullBinaryTree[A, B]] {
      type Node = FullBinaryTree[A, B]#NodeRef
      type NodeLabel = A
      type LeafLabel = B

      def root(t: FullBinaryTree[A, B]): Option[Node] = t.root
      def isLeaf(node: Node): Boolean = node.isLeaf
      def nodeLabel(node: Node): NodeLabel = node.nodeLabel
      def leafLabel(node: Node): LeafLabel = node.leafLabel
      def leftChild(node: Node): Node = node.leftChild
      def rightChild(node: Node): Node = node.rightChild
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
  def apply[T](tree: T)(implicit ev: FullBinaryTreeOps.WithLayout[T]): FullBinaryTree[ev.treeOps.NodeLabel, ev.treeOps.LeafLabel] = {
    import ev._
    import treeOps._

    val bitsBldr = Bitset.newBuilder
    val leafBldr = Bitset.newBuilder
    val nodeLabelBldr = Layout[NodeLabel].newBuilder
    val leafLabelBldr = Layout[LeafLabel].newBuilder

    def build(nodes: Queue[Option[Node]]): Unit =
      if (nodes.nonEmpty) {
        nodes.dequeue match {
          case (None, rest) =>
            bitsBldr += false
            build(rest)
          case (Some(node), rest) =>
            bitsBldr += true
            if (treeOps.isLeaf(node)) {
              leafBldr += true
              leafLabelBldr += treeOps.leafLabel(node)
              build(rest.enqueue(None).enqueue(None))
            } else {
              leafBldr += false
              nodeLabelBldr += treeOps.nodeLabel(node)
              build(rest.enqueue(Some(treeOps.leftChild(node))).enqueue(Some(treeOps.rightChild(node))))
            }
        }
      }

    tree.root match {
      case Some(node) =>
        build(Queue(Some(node)))
        new FullBinaryTree(bitsBldr.result(), leafBldr.result(),
          nodeLabelBldr.result(), leafLabelBldr.result())

      case None =>
        FullBinaryTree.empty
    }
  }
}
