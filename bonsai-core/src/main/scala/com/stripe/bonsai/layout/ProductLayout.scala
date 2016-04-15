package com.stripe.bonsai
package layout

import java.io.{ DataOutput, DataInput }

class ProductLayout[A, B, C](
  leftLayout: Layout[A],
  rightLayout: Layout[B],
  unpack: C => (A, B),
  pack: (A, B) => C
) extends Layout[C] {
  def newBuilder: ProductBuilder[A, B, C] =
    new ProductBuilder[A, B, C](
      leftLayout.newBuilder,
      rightLayout.newBuilder,
      unpack, pack
    )

  def isSafeToCast(vec: Vec[_]): Boolean = vec match {
    case ProductVec(left, right, _) =>
      leftLayout.isSafeToCast(left) && rightLayout.isSafeToCast(right)
    case _ =>
      false
  }

  def write(vec: Vec[C], out: DataOutput): Unit = {
    val ProductVec(left, right, _) = recast(vec)
    out.writeByte(ProductLayout.SplitEncoding)
    leftLayout.write(left, out)
    rightLayout.write(right, out)
  }

  def read(in: DataInput): Vec[C] = {
    in.readByte() match {
      case ProductLayout.SplitEncoding =>
        val left = leftLayout.read(in)
        val right = rightLayout.read(in)
        ProductVec(left, right, pack)

      case _ =>
        throw new java.io.IOException("unsupported encoding for product2 layout")
    }
  }

  private def recast(vec: Vec[C]): ProductVec[A, B, C] = {
    if (isSafeToCast(vec)) {
      vec.asInstanceOf[ProductVec[A, B, C]]
    } else {
      (newBuilder ++= vec).result()
    }
  }
}

object ProductLayout {
  final val SplitEncoding = 1.toByte
}

class ProductBuilder[A, B, C](
  leftBldr: VecBuilder[A],
  rightBldr: VecBuilder[B],
  unpack: C => (A, B),
  pack: (A, B) => C
) extends VecBuilder[C] {
  def +=(that: C) = {
    val (a, b) = unpack(that)
    leftBldr += a
    rightBldr += b
    this
  }

  def clear(): Unit = {
    leftBldr.clear()
    rightBldr.clear()
  }

  def result(): ProductVec[A, B, C] =
    new ProductVec[A, B, C](leftBldr.result(), rightBldr.result(), pack)
}

case class ProductVec[A, B, C](
  left: Vec[A],
  right: Vec[B],
  pack: (A, B) => C
) extends Vec[C] {
  def size: Int = left.size
  def apply(index: Int): C = pack(left(index), right(index))
}
