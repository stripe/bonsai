package com.stripe.bonsai
package layout

import java.io.{ DataOutput, DataInput }

case class OptionalLayout[A](layout: Layout[A]) extends Layout[Option[A]] {
  def newBuilder: OptionalBuilder[A] =
    new OptionalBuilder[A](layout.newBuilder)

  def write(vec: Vec[Option[A]], out: DataOutput): Unit = {
    val OptionalVec(bitset, underlying) = recast(vec)
    out.writeByte(OptionalLayout.BitSetEncoding)
    layout.write(underlying, out)
    out.writeInt(bitset.length)
    IndexedBitSet.write(out, bitset)
  }

  def read(in: DataInput): Vec[Option[A]] = {
    in.readByte() match {
      case DisjunctionLayout.SplitEncoding =>
        val underlying = layout.read(in)
        val length = in.readInt()
        val bitset = IndexedBitSet.read(in, length)
        OptionalVec(bitset, underlying)

      case _ =>
        throw new java.io.IOException("unsupported encoding for optional layout")
    }
  }

  def isSafeToCast(vec: Vec[_]): Boolean = vec match {
    case OptionalVec(_, underlying) =>
      layout.isSafeToCast(underlying)
    case _ =>
      false
  }

  private def recast(vec: Vec[Option[A]]): OptionalVec[A] = {
    if (isSafeToCast(vec)) {
      vec.asInstanceOf[OptionalVec[A]]
    } else {
      (newBuilder ++= vec).result()
    }
  }
}

object OptionalLayout {
  final val BitSetEncoding = 1.toByte
}

class OptionalBuilder[A](bldr: VecBuilder[A]) extends VecBuilder[Option[A]] {
  val bitsetBldr = IndexedBitSet.newBuilder

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

  def result(): OptionalVec[A] =
    new OptionalVec[A](bitsetBldr.result(), bldr.result())
}

case class OptionalVec[A](bitset: IndexedBitSet, vec: Vec[A]) extends Vec[Option[A]] {
  def size: Int = bitset.length
  def apply(index: Int): Option[A] =
    if (bitset(index)) Some(vec(bitset.rank(index) - 1))
    else None
}
