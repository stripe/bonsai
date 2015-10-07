package com.stripe.bonsai
package layout

case class DisjunctionLayout[A, B, C](
  leftLayout: Layout[A],
  rightLayout: Layout[B],
  unpack: C => Either[A, B],
  mkLeft: A => C,
  mkRight: B => C
) extends Layout[C] {
  def newBuilder: VecBuilder[C] =
    new DisjunctionBuilder[A, B, C](
      leftLayout.newBuilder,
      rightLayout.newBuilder,
      unpack, mkLeft, mkRight
    )
}

class DisjunctionBuilder[A, B, C](
  leftBldr: VecBuilder[A],
  rightBldr: VecBuilder[B],
  unpack: C => Either[A, B],
  mkLeft: A => C,
  mkRight: B => C
) extends VecBuilder[C] {
  val bitsetBldr = new BitsetBuilder

  def +=(that: C) = {
    unpack(that) match {
      case Left(a) =>
        bitsetBldr += true
        leftBldr += a

      case Right(b) =>
        bitsetBldr += false
        rightBldr += b
    }
    this
  }

  def clear(): Unit = {
    bitsetBldr.clear()
    leftBldr.clear()
    rightBldr.clear()
  }

  def result(): Vec[C] =
    new DisjunctionVec[A, B, C](
      bitsetBldr.result(), leftBldr.result(), rightBldr.result(),
      mkLeft, mkRight
    )
}

class DisjunctionVec[A, B, C](
  bitset: Bitset,
  left: Vec[A],
  right: Vec[B],
  mkLeft: A => C,
  mkRight: B => C
) extends Vec[C] {
  def size: Int = left.size + right.size
  def apply(index: Int): C = {
    if (bitset(index)) {
      mkLeft(left(bitset.rank(index) - 1))
    } else {
      mkRight(right(index - bitset.rank(index)))
    }
  }
}
