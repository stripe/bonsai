package com.stripe.bonsai
package layout

class TransformedLayout[A, B](
  underlying: Layout[A],
  prepare: B => A,
  present: A => B
) extends Layout[B] {
  def newBuilder: VecBuilder[B] =
    new TransformedBuilder[A, B](
      underlying.newBuilder,
      prepare, present
    )
}

class TransformedBuilder[A, B](
  bldr: VecBuilder[A],
  prepare: B => A,
  present: A => B
) extends VecBuilder[B] {
  def +=(that: B) = {
    bldr += prepare(that)
    this
  }

  def clear(): Unit = bldr.clear()

  def result(): Vec[B] = bldr.result().map(present)
}
