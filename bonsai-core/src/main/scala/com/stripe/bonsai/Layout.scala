package com.stripe.bonsai

import scala.language.implicitConversions

import scala.reflect.ClassTag

import com.stripe.bonsai.layout._

trait Layout[A] {
  def empty: Vec[A] = newBuilder.result()

  def newBuilder: VecBuilder[A]

  def zip[B](that: Layout[B]): Layout[(A, B)] = Layout.join(this, that)

  def transform[B](f: A => B, g: B => A): Layout[B] = Layout.transform(this)(f, g)

  def optional: Layout[Option[A]] = Layout.optional(this)
}

trait LayoutLow1 {
  implicit def denseVector[A]: Layout[A] = new DenseIndexedSeqLayout[Vector, A]
}

trait LayoutLow2 extends LayoutLow1 {
  implicit def denseArray[A: ClassTag]: Layout[A] = new DenseArrayLayout[A]
}

object Layout extends LayoutLow2 {
  def apply[A](implicit layout: Layout[A]): Layout[A] = layout

  implicit def optional[A](implicit layout: Layout[A]): Layout[Option[A]] = new OptionalLayout(layout)

  def transform[A, B](layout: Layout[A])(f: A => B, g: B => A): Layout[B] =
    new TransformedLayout(layout, g, f)

  /**
   * Returns a data store that can store either A or B and requires only
   * 1.37n bits additional data (+ O(1) pointers).
   */
  implicit def either[A, B](implicit lhs: Layout[A], rhs: Layout[B]): Layout[Either[A, B]] =
    new DisjunctionLayout[A, B, Either[A, B]](lhs, rhs, identity, Left(_), Right(_))

  implicit def join[A, B](implicit lhs: Layout[A], rhs: Layout[B]): Layout[(A, B)] =
    new ProductLayout[A, B, (A, B)](lhs, rhs, identity, _ -> _)
}
