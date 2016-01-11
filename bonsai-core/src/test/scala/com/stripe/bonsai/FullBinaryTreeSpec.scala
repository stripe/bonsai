package com.stripe.bonsai

import org.scalatest.{ WordSpec, Matchers }
import org.scalatest.prop.{ Checkers, PropertyChecks }

class FullBinaryTreeSpec extends WordSpec with Matchers with Checkers with PropertyChecks {

  val ops = FullBinaryTreeOps[FullBinaryTree[Int, Int], Int, Int]

  type BTII = FullBinaryTree[Int, Int]
  type GTI = GenericBinTree[Int]

  def doTree[A](t: BTII)(f: ops.Node => A): A =
    ops.root(t) match {
      case Some(n) => f(n)
      case None => sys.error("all our trees are supposed to have root nodes")
    }

  def sumNode(n: ops.Node): Long =
    ops.foldNode(n)((lc, rc, n) => n + sumNode(lc) + sumNode(rc), n => n)

  def nodeElems(n: ops.Node): Set[Int] =
    ops.foldNode(n)((lc, rc, n) => Set(n) | nodeElems(lc) | nodeElems(rc), Set(_))

  def minNode(n: ops.Node): Int =
    ops.foldNode(n)((lc, rc, n) => (n min (minNode(lc) min minNode(rc))), n => n)

  "FullBinaryTree.apply" should {
    "copy structure of tree" in {
      forAll { (tree: GTI) =>
        GenericBinTree.fromTree(FullBinaryTree(tree)) shouldBe Some(tree)
      }
    }

    def buildTreeAndChildren(x: Int, gtrees: Option[(GTI, GTI)]): (BTII, List[BTII]) = {
      val trees = gtrees match {
        case Some((lc, rc)) => FullBinaryTree(lc) :: FullBinaryTree(rc) :: Nil
        case None => Nil
      }
      (FullBinaryTree(GenericBinTree(x, gtrees)), trees)
    }

    "sum with tree ops" in {
      forAll { (x: Int, gtrees: Option[(GTI, GTI)]) =>
        val (tree, trees) = buildTreeAndChildren(x, gtrees)
        val expected = trees.foldLeft(x.toLong)((total, t) => total + doTree(t)(sumNode))
        doTree(tree)(sumNode) shouldBe expected
      }
    }

    "elems with tree ops" in {
      forAll { (x: Int, gtrees: Option[(GTI, GTI)]) =>
        val (tree, trees) = buildTreeAndChildren(x, gtrees)
        val elems = trees.foldLeft(Set(x))((xs, t) => xs | doTree(t)(nodeElems))
        doTree(tree)(nodeElems) shouldBe elems
        doTree(tree)(minNode) shouldBe elems.min
      }
    }
  }
}
