package com.stripe.bonsai
package layout

case class OptionalLayout[A](layout: Layout[A]) extends Layout[Option[A]] {
  def newBuilder: VecBuilder[Option[A]] =
    new OptionalBuilder[A](layout.newBuilder)
}

class OptionalBuilder[A](bldr: VecBuilder[A]) extends VecBuilder[Option[A]] {
  val bitsetBldr = new BitsetBuilder

  def +=(opt: Option[A]) = {
    opt match {
      case Some(a) =>
        bldr += a
        bitsetBldr += true

      case None => 
        bitsetBldr += false
    }
    this
  }

  def clear(): Unit = {
    bitsetBldr.clear()
    bldr.clear()
  }

  def result(): Vec[Option[A]] =
    new OptionalVec[A](bitsetBldr.result(), bldr.result())
}

class OptionalVec[A](bitset: Bitset, vec: Vec[A]) extends Vec[Option[A]] {
  def size: Int = bitset.length
  def apply(index: Int): Option[A] =
    if (bitset(index)) Some(vec(bitset.rank(index)))
    else None
}
