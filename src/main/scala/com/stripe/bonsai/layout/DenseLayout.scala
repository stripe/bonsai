package com.stripe.bonsai
package layout

import scala.language.higherKinds

import scala.collection.mutable.{ ArrayBuilder, Builder }
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

case class DenseIndexedSeqLayout[CC[A] <: IndexedSeq[A], A](
  implicit cbf: CanBuildFrom[Nothing, A, CC[A]]
) extends Layout[A] {
  def newBuilder =
    new DenseBuilder[A, CC[A]](cbf(), DenseIndexedSeqVec(_))
}

case class DenseArrayLayout[A: ClassTag]() extends Layout[A] {
  def newBuilder =
    new DenseBuilder[A, Array[A]](ArrayBuilder.make[A](), DenseArrayVec(_))
}

class DenseBuilder[A, To](
  bldr: Builder[A, To],
  f: To => Vec[A]
) extends VecBuilder[A] {
  def +=(a: A) = {
    bldr += a
    this
  }
  def clear(): Unit = bldr.clear()
  def result(): Vec[A] = f(bldr.result())
}

case class DenseIndexedSeqVec[A](vec: IndexedSeq[A]) extends Vec[A] {
  def size: Int = vec.size
  def apply(index: Int): A = vec(index)
}

case class DenseArrayVec[@specialized A](vec: Array[A]) extends Vec[A] {
  def size: Int = vec.size
  def apply(index: Int): A = vec(index)
}
