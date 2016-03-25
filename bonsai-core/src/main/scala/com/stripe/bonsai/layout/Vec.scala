package com.stripe.bonsai
package layout

import scala.collection.mutable.Builder
import scala.reflect.ClassTag

trait Vec[@specialized A] {
  def size: Int
  def apply(index: Int): A

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
}

object Vec {
  def apply[A](as: A*)(implicit layout: Layout[A]): Vec[A] = {
    val bldr = layout.newBuilder
    bldr ++= as
    bldr.result()
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
