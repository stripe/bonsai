package com.stripe.bonsai

import scala.reflect.ClassTag

trait TreeOps[T] { ops =>
  type Node
  type Data

  def root(t: T): Option[Node]
  def children(node: Node): Iterable[Node]
  def data(node: Node): Data

  implicit class TreeTreeOps(tree: T) {
    def root: Option[Node] = ops.root(tree)
  }

  implicit class TreeNodeOps(node: Node) {
    def children: Iterable[Node] = ops.children(node)
    def data: Data = ops.data(node)
  }
}

object TreeOps {
  type Aux[T, N, D] = TreeOps[T] {
    type Node = N
    type Data = D
  }

  trait WithDataClassTag[T] {
    implicit val treeOps: TreeOps[T]
    implicit val classTag: ClassTag[treeOps.Data]
  }

  implicit def WithDataClassTag[T, N, D](implicit ops: Aux[T, N, D], ct: ClassTag[D]): WithDataClassTag[T] =
    new WithDataClassTag[T] {
      val treeOps = ops
      val classTag = ct
    }
}
