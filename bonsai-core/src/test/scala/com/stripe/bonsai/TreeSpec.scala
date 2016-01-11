package com.stripe.bonsai

import org.scalatest.{ WordSpec, Matchers }
import org.scalatest.prop.{ Checkers, PropertyChecks }

class TreeSpec extends WordSpec with Matchers with Checkers with PropertyChecks {
  val ops = TreeOps[Tree[Int], Int]

  def sumTree(g: Tree[Int]): Long =
    ops.reduce[Long](g)(_ + _.sum).getOrElse(0L)

  def treeElems(g: Tree[Int]): Set[Int] =
    ops.reduce[Set[Int]](g) { (n, as) =>
      as.foldLeft(Set(n))(_ | _)
    } getOrElse(Set.empty)

  def treeMin(g: Tree[Int]): Option[Int] =
    ops.reduce[Int](g) { (n, as) =>
      if (as.isEmpty) n else n min as.min
    }

  "Tree.apply" should {
    "copy structure of tree" in {
      forAll { (tree: GenericTree[Int]) =>
        GenericTree.fromTree(Tree(tree)) shouldBe Some(tree)
      }
    }

    "sum with tree ops" in {
      forAll { (x: Int, gtrees: List[GenericTree[Int]]) =>
        val trees = gtrees.map(Tree(_))
        val tree = Tree(GenericTree(x, gtrees))
        val expected = trees.foldLeft(x.toLong) { (t, n) => t + sumTree(n) }
        sumTree(tree) shouldBe expected
      }
    }

    "elems with tree ops" in {
      forAll { (x: Int, gtrees: List[GenericTree[Int]]) =>
        val trees = gtrees.map(Tree(_))
        val tree = Tree(GenericTree(x, gtrees))

        val elems = trees.foldLeft(Set(x))((t, n) => t | treeElems(n))
        treeElems(tree) shouldBe elems
        treeMin(tree) shouldBe Some(elems.min)
      }
    }
  }
}
