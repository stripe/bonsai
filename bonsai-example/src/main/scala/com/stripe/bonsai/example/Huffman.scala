package com.stripe.bonsai
package example

import org.github.jamm.MemoryMeter

import scala.collection.BitSet
import scala.collection.mutable.PriorityQueue
import scala.util.Random.nextGaussian

object Huffman extends App {
  sealed trait HuffmanTree[+A]

  object HuffmanTree {
    case class Branch[+A](zero: HuffmanTree[A], one: HuffmanTree[A]) extends HuffmanTree[A]
    case class Leaf[+A](value: A) extends HuffmanTree[A]

    def apply[A](symbols: Map[A, Double]): HuffmanTree[A] = {
      require(symbols.nonEmpty)

      val queue = new PriorityQueue[(HuffmanTree[A], Double)]()(Ordering.by(-_._2))

      // Initialize the queue with leaf nodes.
      symbols.foreach { case (symbol, weight) =>
        queue.enqueue(Leaf(symbol) -> weight)
      }

      // Iteratively build up optimal Huffman tree.
      while (queue.size > 1) {
        val (t0, w0) = queue.dequeue()
        val (t1, w1) = queue.dequeue()
        queue.enqueue(Branch(t0, t1) -> (w0 + w1))
      }

      // Return the final tree.
      queue.dequeue()._1
    }

    implicit def huffmanTreeOps[A] = new TreeOps[HuffmanTree[A]] {
      type Node = HuffmanTree[A]
      type Label = Option[A]

      def root(tree: HuffmanTree[A]): Option[HuffmanTree[A]] = Some(tree)
      def children(tree: HuffmanTree[A]): Iterable[HuffmanTree[A]] = tree match {
        case Branch(l, r) => l :: r :: Nil
        case _ => Nil
      }
      def label(tree: HuffmanTree[A]): Label = tree match {
        case Leaf(value) => Some(value)
        case _ => None
      }
    }
  }

  implicit class HuffmanTreeOps[T, A](tree: T)(implicit treeOps: TreeOps.Aux[T, Option[A]]) {
    import HuffmanTree.{ Branch, Leaf }
    import treeOps._

    def decode(bits: BitSet, len: Int): Vector[A] = {
      val root = tree.root.get
      val (_, result) = (0 until len)
        .foldLeft((root, Vector.empty[A])) { case ((node, acc), i) =>
          node.label match {
            case Some(value) => (root, acc :+ value)
            case None if bits(i) => (node.children.head, acc)
            case None => (node.children.iterator.drop(1).next, acc)
          }
        }
      result
    }
  }

  val symbols = Map(('a' to 'z').map { symbol =>
    symbol -> math.abs(nextGaussian)
  }: _*)
  val bigTree = HuffmanTree(symbols)
  val smallTree = Tree(bigTree)

  val meter = new MemoryMeter()
  val bigTreeSize = meter.measureDeep(bigTree)
  val smallTreeSize = meter.measureDeep(smallTree)
  println(s"big tree:   ${bigTreeSize} bytes")
  println(s"small tree: ${smallTreeSize} bytes")
  println(f"${bigTreeSize / smallTreeSize.toDouble}%.1fx reduction")
}
