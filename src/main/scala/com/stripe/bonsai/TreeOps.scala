package com.stripe.bonsai

import scala.reflect.ClassTag

/**
 * TreeOps is a type class for abstracting over arbitrary trees, allowing
 * traversal and data access. In general, it assumes that we can quickly access
 * the children and label of any given node.
 *
 * There are some convenience implicit methods added onto trees and nodes that
 * can be accessed by importing tree ops. For instance, say we wanted to add an
 * implicit method to any type of tree that let's us find some node in a tree.
 *
 * {{{
 * implicit class NodeFinder[T](tree: T)(implicit treeOps: TreeOps[T]) {
 * 
 *   // Brings in Node/Label types, and implicit classes.
 *   import treeOps._
 * 
 *   // Returns the first node (DFS) where the predicate is true.
 *   def find(f: Label => Boolean): Option[Node] = {
 *     def recur(node: Node): Option[Node] = {
 *       // treeOps.TreeNodeOps brings in the Node#label implicit method.
 *       if (f(node.label)) {
 *         node
 *       } else {
 *         // treeOps.TreeNodeOps also brings in Node#children implicit method.
 *         node.children.iterator.map(recur).find(_.isDefined).flatten
 *       }
 *     }
 * 
 *     // treeOps.TreeTreeOps brings in T#root.
 *     recur(tree.root)
 *   }
 * }
 * }}}
 */
trait TreeOps[T] { ops =>

  /** The type of the nodes in the tree. */
  type Node

  /** The type of the label attached to each node. */
  type Label

  /**
   * Returns the root node of the tree.
   */
  def root(t: T): Option[Node]

  /**
   * Returns all the direct children of the given node. The order may or may
   * not matter. TreeOps does not provide any guarantees here.
   */
  def children(node: Node): Iterable[Node]

  /**
   * Returns the label attached to the given node.
   */
  def label(node: Node): Label

  def fold[A](f: (Label, Iterable[A]) => A)(node: Node): A =
    f(label(node), children(node).map(fold(f)))

  implicit class TreeTreeOps(tree: T) {
    def root: Option[Node] = ops.root(tree)
    def fold[A](f: (Label, Iterable[A]) => A): Option[A] = root.map(ops.fold(f))
  }

  implicit class TreeNodeOps(node: Node) {
    def children: Iterable[Node] = ops.children(node)
    def label: Label = ops.label(node)
    def fold[A](f: (Label, Iterable[A]) => A): A = ops.fold(f)(node)
  }
}

object TreeOps {

  /**
   * A type alias for `TreeOps` that let's you use the `Node` and `Label` types
   * with Scala's type inference / implicit lookup. You normally shouldn't need
   * this, but is invaluable when you do.
   */
  type Aux[T, N, D] = TreeOps[T] {
    type Node = N
    type Label = D
  }

  trait WithLayout[T] {
    implicit val treeOps: TreeOps[T]
    implicit val layout: Layout[treeOps.Label]
  }

  object WithLayout {
    implicit def mkWithLayout[T, N, D](implicit ops: Aux[T, N, D], lt: Layout[D]): WithLayout[T] =
      new WithLayout[T] {
        val treeOps = ops
        val layout = lt
      }
  }
}
