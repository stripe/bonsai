package com.stripe.bonsai

import org.scalatest.{ WordSpec, Matchers }
import org.scalatest.prop.Checkers

//import org.scalacheck.Arbitrary._
//import org.scalacheck.Prop._

class TreeSpec extends WordSpec with Matchers with Checkers {
  "Tree.apply" should {
    "copy structure of tree" in {
      check { (tree: GenericTree[Int]) =>
        GenericTree.fromTree(Tree(tree)).get == tree
      }
    }
  }
}
