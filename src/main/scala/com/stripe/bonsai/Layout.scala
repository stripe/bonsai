package com.stripe.bonsai

import scala.reflect.ClassTag

import com.stripe.bonsai.layout._

trait Layout[A] {
  def newBuilder: VecBuilder[A]

  def zip[B](that: Layout[B]): Layout[(A, B)] = Layout.join(this, that)

  def transform[B](f: A => B, g: B => A): Layout[B] = Layout.transform(this)(f, g)

  def optional: Layout[Option[A]] = Layout.optional(this)
}

trait LayoutLow {
  implicit def denseVector[A] = new DenseIndexedSeqLayout[Vector, A]
}

object Layout {
  implicit def optional[A](layout: Layout[A]): Layout[Option[A]] = new OptionalLayout(layout)

  implicit def denseArray[A: ClassTag] = new DenseArrayLayout[A]

  def transform[A, B](layout: Layout[A])(f: A => B, g: B => A): Layout[B] =
    new TransformedLayout(layout, g, f)

  /**
   * Returns a data store that can store either A or B and requires only
   * 1.37n bits additional data (+ O(1) pointers).
   */
  implicit def either[A, B](lhs: Layout[A], rhs: Layout[B]): Layout[Either[A, B]] =
    new DisjunctionLayout[A, B, Either[A, B]](lhs, rhs, identity, Left(_), Right(_))

  implicit def join[A, B](lhs: Layout[A], rhs: Layout[B]): Layout[(A, B)] =
    new ProductLayout[A, B, (A, B)](lhs, rhs, identity, _ -> _)
}
