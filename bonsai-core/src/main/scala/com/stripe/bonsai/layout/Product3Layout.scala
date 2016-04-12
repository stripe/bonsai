package com.stripe.bonsai
package layout

import java.io.{ DataOutput, DataInput }

class Product3Layout[A, B, C, D](
  layoutA: Layout[A],
  layoutB: Layout[B],
  layoutC: Layout[C],
  unpack: D => (A, B, C),
  pack: (A, B, C) => D
) extends Layout[D] {
  def newBuilder: Product3Builder[A, B, C, D] =
    new Product3Builder[A, B, C, D](
      layoutA.newBuilder,
      layoutB.newBuilder,
      layoutC.newBuilder,
      unpack, pack
    )

  def isSafeToCast(vec: Vec[_]): Boolean = vec match {
    case Product3Vec(a, b, c, _) =>
      layoutA.isSafeToCast(a) && layoutB.isSafeToCast(b) && layoutC.isSafeToCast(c)
    case _ =>
      false
  }

  def write(vec: Vec[D], out: DataOutput): Unit = {
    val Product3Vec(as, bs, cs, _) = recast(vec)
    layoutA.write(as, out)
    layoutB.write(bs, out)
    layoutC.write(cs, out)
  }

  def read(in: DataInput): Vec[D] = {
    val as = layoutA.read(in)
    val bs = layoutB.read(in)
    val cs = layoutC.read(in)
    Product3Vec(as, bs, cs, pack)
  }

  private def recast(vec: Vec[D]): Product3Vec[A, B, C, D] = {
    if (isSafeToCast(vec)) {
      vec.asInstanceOf[Product3Vec[A, B, C, D]]
    } else {
      (newBuilder ++= vec).result()
    }
  }
}

class Product3Builder[A, B, C, D](
  aBldr: VecBuilder[A],
  bBldr: VecBuilder[B],
  cBldr: VecBuilder[C],
  unpack: D => (A, B, C),
  pack: (A, B, C) => D
) extends VecBuilder[D] {
  def +=(that: D) = {
    val (a, b, c) = unpack(that)
    aBldr += a
    bBldr += b
    cBldr += c
    this
  }

  def clear(): Unit = {
    aBldr.clear()
    bBldr.clear()
    cBldr.clear()
  }

  def result(): Product3Vec[A, B, C, D] =
    new Product3Vec[A, B, C, D](aBldr.result(), bBldr.result(), cBldr.result(), pack)
}

case class Product3Vec[A, B, C, D](
  as: Vec[A],
  bs: Vec[B],
  cs: Vec[C],
  pack: (A, B, C) => D
) extends Vec[D] {
  def size: Int = as.size
  def apply(index: Int): D = pack(as(index), bs(index), cs(index))
}
