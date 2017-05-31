package com.stripe.bonsai
package layout

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

trait Vec[@specialized A] {
  def size: Int
  def apply(index: Int): A

  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = size != 0

  /**
   * Returns an equivalent `Vec[A]` that is backed by a single `Array`.
   */
  def inflate(implicit ct: ClassTag[A]): Vec[A] = {
    val xs = toArray
    new Vec[A] {
      def size: Int = xs.length
      def apply(i: Int): A = xs(i)
    }
  }

  /**
   * Returns an equivalent `Vec[A]` that is laid out efficiently in memory.
   */
  def deflate(implicit layout: Layout[A]): Vec[A] =
    (layout.newBuilder ++= this).result()

  def foreach(f: A => Unit): Unit = {
    var i = 0
    while (i < size) {
      f(apply(i))
      i += 1
    }
  }

  def toArray(implicit ct: ClassTag[A]): Array[A] = {
    val arr = new Array[A](size)
    var i = 0
    while (i < arr.length) {
      arr(i) = apply(i)
      i += 1
    }
    arr
  }

  def map[B](f: A => B): Vec[B] =
    new MappedVec[A, B](this, f)

  override def equals(that: Any): Boolean = that match {
    case (that: Vec[_]) if size == that.size =>
      var i = 0
      while (i < size) {
        if (apply(i) != that(i))
          return false
        i += 1
      }
      true

    case _ => false
  }

  override def hashCode: Int = {
    var i = 0
    var h = MurmurHash3.symmetricSeed
    while (i < size) {
      h = MurmurHash3.mix(h, apply(i).hashCode)
      i += 1
    }
    MurmurHash3.finalizeHash(h, i)
  }
}

object Vec {
  def apply[A](as: A*)(implicit layout: Layout[A]): Vec[A] = {
    val bldr = layout.newBuilder
    bldr ++= as
    bldr.result()
  }

  def fromVector[A](values: Vector[A]): Vec[A] =
    new Vec[A] {
      def size: Int = values.size
      def apply(i: Int): A = values(i)
    }
}

trait VecBuilder[A] extends Builder[A, Vec[A]] {
  def ++=(that: Vec[A]): this.type = {
    that.foreach { a => this += a }
    this
  }
}

case class MappedVec[A, B](vec: Vec[A], f: A => B) extends Vec[B] {
  def size: Int = vec.size
  def apply(index: Int): B = f(vec(index))
}

case class ColVec[A, Repr](
  offsets: Vec[Int],
  values: Vec[A]
)(implicit cbf: CanBuildFrom[Nothing, A, Repr]) extends Vec[Repr] {
  def size: Int = offsets.size
  def apply(i: Int): Repr = {
    val start = offsets(i)
    val end = if ((i + 1) == size) values.size else offsets(i + 1)
    val bldr = cbf()
    Iterator.range(start, end).foreach { i =>
      bldr += values(i)
    }
    bldr.result()
  }
}
