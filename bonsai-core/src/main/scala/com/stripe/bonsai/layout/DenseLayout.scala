package com.stripe.bonsai
package layout

import scala.language.higherKinds

import scala.collection.mutable.{ ArrayBuilder, Builder }
import scala.collection.generic.CanBuildFrom
import scala.reflect.{ ClassTag, classTag }

import java.io.{ DataOutput, DataInput }

sealed abstract class DenseArrayLayout[A: ClassTag](
  cons: Array[A] => Vec[A],
  readArray: (DataInput, Array[A]) => Unit
) extends Layout[A] {
  def newBuilder =
    new DenseBuilder[A, Array[A]](ArrayBuilder.make[A](), cons)

  def isSafeToCast(vec: Vec[_]): Boolean = vec match {
    case (vec: DenseVec[_]) =>
      // If the arrays match up, then it's all good.
      vec.vec.getClass == classTag[A].wrap.runtimeClass
    case _ =>
      false
  }

  def read(in: DataInput): Vec[A] = {
    val length = in.readInt()
    val arr = classTag[A].newArray(length)
    readArray(in, arr)
    cons(arr)
  }

  def write(vec: Vec[A], out: DataOutput): Unit = {
    val arr: Array[_] = vec match {
      case (vec: DenseVec[_]) => vec.vec
      case _ => vec.toArray
    }

    out.writeInt(arr.length)

    arr match {
      case arr: Array[Boolean] =>
        var i = 0
        while (i < arr.length) {
          val b: Boolean = arr(i)
          out.writeBoolean(arr(i))
          i += 1
        }

      case arr: Array[Byte] =>
        var i = 0
        while (i < arr.length) {
          out.writeByte(arr(i))
          i += 1
        }

      case arr: Array[Short] =>
        var i = 0
        while (i < arr.length) {
          out.writeShort(arr(i))
          i += 1
        }

      case arr: Array[Int] =>
        var i = 0
        while (i < arr.length) {
          out.writeInt(arr(i))
          i += 1
        }

      case arr: Array[Long] =>
        var i = 0
        while (i < arr.length) {
          out.writeLong(arr(i))
          i += 1
        }

      case arr: Array[Float] =>
        var i = 0
        while (i < arr.length) {
          out.writeFloat(arr(i))
          i += 1
        }

      case arr: Array[Double] =>
        var i = 0
        while (i < arr.length) {
          out.writeDouble(arr(i))
          i += 1
        }

      case arr: Array[Char] =>
        var i = 0
        while (i < arr.length) {
          out.writeChar(arr(i))
          i += 1
        }
    }
  }
}

object DenseArrayLayout {
  def readBoolean(in: DataInput, arr: Array[Boolean]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readBoolean()
    }
  }

  def readByte(in: DataInput, arr: Array[Byte]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readByte()
    }
  }

  def readShort(in: DataInput, arr: Array[Short]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readShort()
    }
  }

  def readInt(in: DataInput, arr: Array[Int]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readInt()
    }
  }

  def readLong(in: DataInput, arr: Array[Long]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readLong()
    }
  }

  def readFloat(in: DataInput, arr: Array[Float]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readFloat()
    }
  }

  def readDouble(in: DataInput, arr: Array[Double]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readDouble()
    }
  }

  def readChar(in: DataInput, arr: Array[Char]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readChar()
    }
  }
}

import DenseArrayLayout._

case object DenseBooleanLayout extends DenseArrayLayout[Boolean](DenseBooleanVec, readBoolean)
case object DenseByteLayout extends DenseArrayLayout[Byte](DenseByteVec, readByte)
case object DenseShortLayout extends DenseArrayLayout[Short](DenseShortVec, readShort)
case object DenseIntLayout extends DenseArrayLayout[Int](DenseIntVec, readInt)
case object DenseLongLayout extends DenseArrayLayout[Long](DenseLongVec, readLong)
case object DenseFloatLayout extends DenseArrayLayout[Float](DenseFloatVec, readFloat)
case object DenseDoubleLayout extends DenseArrayLayout[Double](DenseDoubleVec, readDouble)
case object DenseCharLayout extends DenseArrayLayout[Char](DenseCharVec, readChar)

class DenseBuilder[A, To](
  bldr: Builder[A, To],
  f: To => Vec[A]
) extends VecBuilder[A] {
  def +=(a: A) = {
    bldr += a
    this
  }
  def clear(): Unit = bldr.clear()
  def result(): Vec[A] = f(bldr.result())
}

sealed trait DenseVec[@specialized A] extends Vec[A] {
  def vec: Array[A]
  def size: Int = vec.length
}

case class DenseBooleanVec(vec: Array[Boolean]) extends DenseVec[Boolean] {
  def apply(index: Int): Boolean = vec(index)
}

case class DenseByteVec(vec: Array[Byte]) extends DenseVec[Byte] {
  def apply(index: Int): Byte = vec(index)
}

case class DenseShortVec(vec: Array[Short]) extends DenseVec[Short] {
  def apply(index: Int): Short = vec(index)
}

case class DenseIntVec(vec: Array[Int]) extends DenseVec[Int] {
  def apply(index: Int): Int = vec(index)
}

case class DenseLongVec(vec: Array[Long]) extends DenseVec[Long] {
  def apply(index: Int): Long = vec(index)
}

case class DenseFloatVec(vec: Array[Float]) extends DenseVec[Float] {
  def apply(index: Int): Float = vec(index)
}

case class DenseDoubleVec(vec: Array[Double]) extends DenseVec[Double] {
  def apply(index: Int): Double = vec(index)
}

case class DenseCharVec(vec: Array[Char]) extends DenseVec[Char] {
  def apply(index: Int): Char = vec(index)
}
