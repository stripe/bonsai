package com.stripe.bonsai
package layout

import java.io.{ DataOutput, DataInput }

class TransformedLayout[A, B](
  layout: Layout[A],
  prepare: B => A,
  present: A => B
) extends Layout[B] {
  def newBuilder: TransformedBuilder[A, B] =
    new TransformedBuilder[A, B](
      layout.newBuilder,
      prepare, present
    )

  def write(vec: Vec[B], out: DataOutput): Unit = {
    val MappedVec(underlying, _) = recast(vec)
    layout.write(underlying, out)
  }

  def read(in: DataInput): Vec[B] = {
    val underlying = layout.read(in)
    MappedVec(underlying, present)
  }

  def isSafeToCast(vec: Vec[_]): Boolean = vec match {
    case MappedVec(underlying, _) =>
      layout.isSafeToCast(underlying)
    case _ =>
      false
  }

  private def recast(vec: Vec[B]): MappedVec[A, B] = {
    if (isSafeToCast(vec)) {
      vec.asInstanceOf[MappedVec[A, B]]
    } else {
      (newBuilder ++= vec).result()
    }
  }
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

  def result(): MappedVec[A, B] =
    MappedVec(bldr.result(), present)
}
