package com.stripe.bonsai

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

case class GenericTree[A](label: A, children: List[GenericTree[A]])

object GenericTree {
  def node[A](label: A, children: GenericTree[A]*): GenericTree[A] =
    GenericTree(label, children.toList)

  def leaf[A](label: A): GenericTree[A] =
    GenericTree(label, Nil)

  implicit def TreeOps[A]: TreeOps[GenericTree[A], A] =
    new TreeOps[GenericTree[A], A] {
      type Node = GenericTree[A]
      def root(t: GenericTree[A]): Option[Node] = Some(t)
      def children(node: Node): List[Node] = node.children
      def label(node: Node): A = node.label
    }

  def fromTree[A](tree: Tree[A]): Option[GenericTree[A]] = {
    def mkTree(node: Tree.NodeRef[A]): GenericTree[A] =
      GenericTree(node.label, node.children.map(mkTree)(collection.breakOut))

    tree.root.map(mkTree)
  }

  implicit def arbitraryTree[A: Arbitrary]: Arbitrary[GenericTree[A]] =
    Arbitrary(genTree(arbitrary[A], 3, 7))

  def genTree[A](genLabel: Gen[A], maxChildren: Int, maxDepth: Int): Gen[GenericTree[A]] =
    if (maxDepth == 1) genLabel.map(GenericTree.leaf) else {
      val k = maxDepth - 1
      for {
        label       <- genLabel
        numChildren <- Gen.choose(0, maxChildren)
        children    <- Gen.listOfN(numChildren, genTree(genLabel, maxChildren, maxDepth - 1))
      } yield GenericTree(label, children)
    }
}

case class GenericBinTree[A](label: A, children: Option[(GenericBinTree[A], GenericBinTree[A])])

object GenericBinTree {
  def branch[A](label: A, left: GenericBinTree[A], right: GenericBinTree[A]): GenericBinTree[A] =
    GenericBinTree(label, Some((left, right)))

  def leaf[A](label: A): GenericBinTree[A] =
    GenericBinTree(label, None)

  implicit def GenericBinTreeOps[A]: FullBinaryTreeOps[GenericBinTree[A], A, A] =
    new FullBinaryTreeOps[GenericBinTree[A], A, A] {
      type Node = GenericBinTree[A]

      def root(t: GenericBinTree[A]): Option[Node] =
        Some(t)

      def foldNode[X](node: Node)(f: (Node, Node, A) => X, g: A => X): X =
        node.children match {
          case Some((lc, rc)) => f(lc, rc, node.label)
          case None => g(node.label)
        }
    }

  def fromTree[A](tree: FullBinaryTree[A, A]): Option[GenericBinTree[A]] = {
    def construct(n: tree.NodeRef): GenericBinTree[A] =
      n.fold({ (lc, rc, a) =>
        GenericBinTree.branch(a, construct(lc), construct(rc))
      }, GenericBinTree.leaf)
    tree.root.map(construct)
  }

  implicit def arbitraryGenericBinTree[A: Arbitrary]: Arbitrary[GenericBinTree[A]] =
    Arbitrary(genGenericBinTree(arbitrary[A], 5))

  def genGenericBinTreeLeaf[A](genLabel: Gen[A]): Gen[GenericBinTree[A]] =
    genLabel.map(GenericBinTree.leaf)

  def genGenericBinTreeBranch[A](genLabel: Gen[A], maxDepth: Int): Gen[GenericBinTree[A]] =
    if (maxDepth == 1) genGenericBinTreeLeaf(genLabel)
    else for {
      label <- genLabel
      left <- genGenericBinTree(genLabel, maxDepth - 1)
      right <- genGenericBinTree(genLabel, maxDepth - 1)
    } yield GenericBinTree.branch(label, left, right)

  def genGenericBinTree[A](genLabel: Gen[A], maxDepth: Int): Gen[GenericBinTree[A]] =
    Gen.frequency(
      7 -> genGenericBinTreeBranch(genLabel, maxDepth),
      2 -> genGenericBinTreeLeaf(genLabel))
}
