package com.stripe.bonsai
package layout

import java.io.{ DataOutput, DataInput }
import scala.collection.IterableLike
import scala.collection.generic.{ CanBuildFrom, IsTraversableLike }

case class ColLayout[A, Repr <: IterableLike[A, Repr]](
  layout: Layout[A],
  offsetsLayout: Layout[Int]
)(implicit
  cbf: CanBuildFrom[Nothing, A, Repr]
) extends Layout[Repr] {
  def newBuilder: VecBuilder[Repr] =
    new DenseBuilder(Vector.newBuilder[Repr], Vec.fromVector)

  def isSafeToCast(vec: Vec[_]): Boolean = false

  def write(vec: Vec[Repr], out: DataOutput): Unit = {
    out.writeByte(ColLayout.OffsetEncoding)
    val offsetsBldr = offsetsLayout.newBuilder
    val bldr = layout.newBuilder
    var offset = 0
    vec.foreach { values =>
      offsetsBldr += offset
      offset += values.size
      bldr ++= values
    }
    offsetsLayout.write(offsetsBldr.result(), out)
    layout.write(bldr.result(), out)
  }

  def read(in: DataInput): Vec[Repr] =
    in.readByte() match {
      case ColLayout.OffsetEncoding =>
        val offsets = offsetsLayout.read(in)
        val values = layout.read(in)
        ColVec(offsets, values)(cbf)

      case e =>
        throw new java.io.IOException(s"unsupported encoding for ColLayout: $e")
    }
}

object ColLayout {
  final val OffsetEncoding = 1.toByte
}
