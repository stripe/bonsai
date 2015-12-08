package com.stripe.bonsai
package layout

import scala.collection.mutable.Builder

trait Vec[@specialized A] {
  def size: Int
  def apply(index: Int): A

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

trait VecBuilder[A] extends Builder[A, Vec[A]]

class MappedVec[A, B](vec: Vec[A], f: A => B) extends Vec[B] {
  def size: Int = vec.size
  def apply(index: Int): B = f(vec(index))
}
