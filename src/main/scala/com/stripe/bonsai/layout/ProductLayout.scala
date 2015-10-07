package com.stripe.bonsai
package layout

class ProductLayout[A, B, C](
  leftLayout: Layout[A],
  rightLayout: Layout[B],
  unpack: C => (A, B),
  pack: (A, B) => C
) extends Layout[C] {
  def newBuilder: VecBuilder[C] =
    new ProductBuilder[A, B, C](
      leftLayout.newBuilder,
      rightLayout.newBuilder,
      unpack, pack
    )
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

  def result(): Vec[C] =
    new ProductVec[A, B, C](leftBldr.result(), rightBldr.result(), pack)
}

class ProductVec[A, B, C](
  left: Vec[A],
  right: Vec[B],
  pack: (A, B) => C
) extends Vec[C] {
  def size: Int = left.size
  def apply(index: Int): C = pack(left(index), right(index))
}
