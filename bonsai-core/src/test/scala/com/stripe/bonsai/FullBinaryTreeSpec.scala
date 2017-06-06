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
    ops.foldNode(n)((n, lc, rc) => n + sumNode(lc) + sumNode(rc), n => n)

  def nodeElems(n: ops.Node): Set[Int] =
    ops.foldNode(n)((n, lc, rc) => Set(n) | nodeElems(lc) | nodeElems(rc), Set(_))

  def minNode(n: ops.Node): Int =
    ops.foldNode(n)((n, lc, rc) => (n min (minNode(lc) min minNode(rc))), n => n)

  "write" should {
    "round-trip through read" in {
      import java.io._

      forAll { (genericTree: GenericBinTree[Int]) =>
        val tree = FullBinaryTree(genericTree)
        val baos = new ByteArrayOutputStream
        FullBinaryTree.write(tree, new DataOutputStream(baos))
        val tree2 = FullBinaryTree.read[Int, Int](new DataInputStream(new ByteArrayInputStream(baos.toByteArray)))
        tree shouldBe tree2
      }
    }
  }

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

        tree.reduce[Set[Int]](Set(_) | _ | _)(Set(_)) shouldBe Some(elems)
      }
    }
  }

  "equals" should {
    "return true on structural equality" in {
      check { (genTree: GTI) =>
        val bt1 = FullBinaryTree(genTree)
        val bt2 = FullBinaryTree(genTree)
        bt1 == bt2 && bt1.hashCode == bt2.hashCode
      }
    }
  }

  "hashCode" should {
    "should agree with equals" in {
      check { (gt1: GTI, gt2: GTI) =>
        val bt1 = FullBinaryTree(gt1)
        val bt2 = FullBinaryTree(gt2)
        bt1.hashCode == bt2.hashCode || bt1 != bt2
      }
    }
  }

  "FullBinaryTreeOps" should {
    "collect leaf labels" in {
      val genTree = GenericBinTree.branch(2, GenericBinTree.leaf(1), GenericBinTree.leaf(3))
      val bt1 = FullBinaryTree(genTree)
      ops.collectLeafLabels(ops.root(bt1).get) shouldBe Set(1, 3)
    }

    "stream all labels" in {
      import GenericBinTree._
      val genTree = branch(0, branch(2, leaf(1), leaf(3)), branch(6, leaf(5), leaf(0)))
      val bt1 = FullBinaryTree(genTree)
      val expected = List(1, 2, 3, 0, 5, 6, 0)
      ops.collectLabelsF(ops.root(bt1).get)(_.merge) shouldBe expected.toSet
    }
  }
}
