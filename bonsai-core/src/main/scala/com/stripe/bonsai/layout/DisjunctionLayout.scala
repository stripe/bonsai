package com.stripe.bonsai
package layout

import java.io.{ DataOutput, DataInput }

case class DisjunctionLayout[A, B, C](
  leftLayout: Layout[A],
  rightLayout: Layout[B],
  unpack: C => Either[A, B],
  mkLeft: A => C,
  mkRight: B => C
) extends Layout[C] {
  def newBuilder: DisjunctionBuilder[A, B, C] =
    new DisjunctionBuilder[A, B, C](
      leftLayout.newBuilder,
      rightLayout.newBuilder,
      unpack, mkLeft, mkRight
    )

  def write(vec: Vec[C], out: DataOutput): Unit = {
    val DisjunctionVec(bitset, left, right, _, _) = recast(vec)
    leftLayout.write(left, out)
    rightLayout.write(right, out)
    IndexedBitSet.write(out, bitset)
  }

  def read(in: DataInput): Vec[C] = {
    val left = leftLayout.read(in)
    val right = rightLayout.read(in)
    val bitset = IndexedBitSet.read(in, left.size + right.size)
    DisjunctionVec(bitset, left, right, mkLeft, mkRight)
  }

  def isSafeToCast(vec: Vec[_]): Boolean = vec match {
    case DisjunctionVec(_, left, right, _, _) =>
      leftLayout.isSafeToCast(left) && rightLayout.isSafeToCast(right)
    case _ =>
      false
  }

  private def recast(vec: Vec[C]): DisjunctionVec[A, B, C] = {
    if (isSafeToCast(vec)) {
      vec.asInstanceOf[DisjunctionVec[A, B, C]]
    } else {
      (newBuilder ++= vec).result()
    }
  }
}

class DisjunctionBuilder[A, B, C](
  leftBldr: VecBuilder[A],
  rightBldr: VecBuilder[B],
  unpack: C => Either[A, B],
  mkLeft: A => C,
  mkRight: B => C
) extends VecBuilder[C] {
  val bitsetBldr = new IndexedBitSetBuilder

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

  def result(): DisjunctionVec[A, B, C] =
    new DisjunctionVec[A, B, C](
      bitsetBldr.result(), leftBldr.result(), rightBldr.result(),
      mkLeft, mkRight
    )
}

case class DisjunctionVec[A, B, C](
  bitset: IndexedBitSet,
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
