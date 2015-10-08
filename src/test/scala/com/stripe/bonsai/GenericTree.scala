package com.stripe.bonsai

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

case class GenericTree[A](label: A, children: List[GenericTree[A]])

object GenericTree {
  def node[A](label: A, children: GenericTree[A]*): GenericTree[A] =
    GenericTree(label, children.toList)

  def leaf[A](label: A): GenericTree[A] =
    GenericTree(label, Nil)

  implicit def GenericTreeOps[A] = new TreeOps[GenericTree[A]] {
    type Node = GenericTree[A]
    type Label = A
    def root(t: GenericTree[A]): Option[Node] = Some(t)
    def children(node: Node): List[Node] = node.children
    def label(node: Node): Label = node.label
  }

  def fromTree[A](tree: Tree[A]): Option[GenericTree[A]] = {
    def mkTree(node: Tree.NodeRef[A]): GenericTree[A] =
      GenericTree(node.label, node.children.map(mkTree)(collection.breakOut))

    tree.root.map(mkTree)
  }

  implicit def arbitraryTree[A: Arbitrary]: Arbitrary[GenericTree[A]] =
    Arbitrary(genTree(arbitrary[A], 5, 12))

  def genTree[A](genLabel: Gen[A], maxChildren: Int, maxDepth: Int): Gen[GenericTree[A]] =
    if (maxDepth == 1) {
      genLabel.map(GenericTree.leaf(_))
    } else {
      val k = maxDepth - 1
      for {
        label       <- genLabel
        numChildren <- Gen.choose(0, maxChildren)
        children    <- Gen.listOfN(numChildren, genTree(genLabel, maxChildren, maxDepth - 1))
      } yield GenericTree(label, children)
    }
}
