package com.stripe.bonsai

trait FullBinaryTreeOps[T] extends TreeOps[T] {
  type LeafLabel
  type NodeLabel
  type Label = Either[NodeLabel, LeafLabel]

  def isLeaf(node: Node): Boolean

  // This is terribly named, since Node/node here mean different things.
  def nodeLabel(node: Node): NodeLabel

  def leafLabel(node: Node): LeafLabel

  // These 2 assume node is not a leaf

  def leftChild(node: Node): Node

  def rightChild(node: Node): Node

  def label(node: Node): Either[NodeLabel, LeafLabel] =
    if (isLeaf(node)) Right(leafLabel(node))
    else Left(nodeLabel(node))

  // We could implement everything in terms of this instead, but I suspect
  // we'd just end up constantly overriding this for perf :\
  def foldLabel[A](node: Node)(f: NodeLabel => A, g: LeafLabel => A): A =
    if (isLeaf(node)) g(leafLabel(node))
    else f(nodeLabel(node))

  def children(node: Node): Iterable[Node] =
    if (isLeaf(node)) Iterable.empty
    else Iterable(leftChild(node), rightChild(node))
}

object FullBinaryTreeOps {
  final def apply[T](implicit ops: FullBinaryTreeOps[T]) = ops

  /**
   * A type alias for `TreeOps` that let's you use the `Node` and `Label` types
   * with Scala's type inference / implicit lookup. You normally shouldn't need
   * this, but is invaluable when you do.
   */
  type Aux[T, A, B] = FullBinaryTreeOps[T] {
    type NodeLabel = A
    type LeafLabel = B
  }

  trait WithLayout[T] {
    implicit val treeOps: FullBinaryTreeOps[T]
    implicit val nodeLayout: Layout[treeOps.NodeLabel]
    implicit val leafLayout: Layout[treeOps.LeafLabel]
  }

  object WithLayout {
    implicit def mkWithLayout[T, A, B](implicit ops: Aux[T, A, B], la: Layout[A], lb: Layout[B]): WithLayout[T] =
      new WithLayout[T] {
        val treeOps = ops
        val nodeLayout = la
        val leafLayout = lb
      }
  }
}
