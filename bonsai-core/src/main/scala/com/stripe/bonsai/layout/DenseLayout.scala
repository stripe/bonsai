package com.stripe.bonsai
package layout

import scala.language.higherKinds

import scala.collection.mutable.{ ArrayBuilder, Builder }
import scala.collection.generic.CanBuildFrom
import scala.reflect.{ ClassTag, classTag }

import java.io.{ DataOutput, DataInput }

sealed abstract class DenseArrayLayout[A: ClassTag](
  cons: Array[A] => Vec[A],
  readArray: (DataInput, Array[A]) => Unit,
  writeArray: (Array[A], DataOutput) => Unit
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
    in.readByte() match {
      case DenseArrayLayout.PlainEncoding =>
        val length = in.readInt()
        val arr = classTag[A].newArray(length)
        readArray(in, arr)
        cons(arr)

      case DenseArrayLayout.ByteDictionaryEncoding =>
        val dictLen = in.readInt()
        require(dictLen <= 256)
        val dict: Array[A] = new Array[A](dictLen)
        readArray(in, dict)
        val encoding: Array[Byte] = new Array[Byte](in.readInt())
        DenseArrayLayout.readByte(in, encoding)
        // Ideally we would just use the dict encoding directly, but
        // this will suffice for now.
        val arr: Array[A] = new Array[A](encoding.length)
        var i = 0
        while (i < arr.length) {
          arr(i) = dict(encoding(i).toInt & 0xFF)
          i += 1
        }
        cons(arr)

      case _ =>
        throw new java.io.IOException("unsupported encoding for dense array layout")
    }
  }

  def write(vec: Vec[A], out: DataOutput): Unit = {
    val arr: Array[A] = vec match {
      case (vec: DenseVec[_]) => vec.vec
      case _ => vec.toArray
    }

    dictionaryFor(arr) match {
      case Some((dict, encoding)) =>
        out.writeByte(DenseArrayLayout.ByteDictionaryEncoding)
        out.writeInt(dict.length)
        writeArray(dict, out)
        out.writeInt(encoding.length)
        DenseArrayLayout.writeByte(encoding, out)

      case None =>
        out.writeByte(DenseArrayLayout.PlainEncoding)
        out.writeInt(arr.length)
        writeArray(arr, out)
    }
  }

  private def dictionaryFor(arr: Array[A]): Option[(Array[A], Array[Byte])] = {
    import scala.collection.mutable.HashMap
    val dict: HashMap[A, Int] = new HashMap
    val encoding: Array[Byte] = new Array[Byte](arr.length)
    var i: Int = 0
    while (i < arr.length) {
      val a = arr(i)
      val key = dict.get(a) match {
        case Some(k) =>
          k
        case None =>
          val k = dict.size
          if (k > 255)
            return None
          dict.put(a, k)
          k
      }
      encoding(i) = key.toByte
      i += 1
    }
    val flatDict: Array[A] = new Array[A](dict.size)
    dict.foreach { case (a, k) =>
      flatDict(k) = a
    }
    Some((flatDict, encoding))
  }
}

object DenseArrayLayout {
  val PlainEncoding = 0.toByte
  val ByteDictionaryEncoding = 1.toByte

  def readBoolean(in: DataInput, arr: Array[Boolean]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readBoolean()
      i += 1
    }
  }

  def readByte(in: DataInput, arr: Array[Byte]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readByte()
      i += 1
    }
  }

  def readShort(in: DataInput, arr: Array[Short]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readShort()
      i += 1
    }
  }

  def readInt(in: DataInput, arr: Array[Int]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readInt()
      i += 1
    }
  }

  def readLong(in: DataInput, arr: Array[Long]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readLong()
      i += 1
    }
  }

  def readFloat(in: DataInput, arr: Array[Float]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readFloat()
      i += 1
    }
  }

  def readDouble(in: DataInput, arr: Array[Double]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readDouble()
      i += 1
    }
  }

  def readChar(in: DataInput, arr: Array[Char]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readChar()
      i += 1
    }
  }

  def readString(in: DataInput, arr: Array[String]): Unit = {
    var i = 0
    while (i < arr.length) {
      arr(i) = in.readUTF()
      i += 1
    }
  }

  def writeBoolean(arr: Array[Boolean], out: DataOutput): Unit = {
    var i = 0
    while (i < arr.length) {
      out.writeBoolean(arr(i))
      i += 1
    }
  }

  def writeByte(arr: Array[Byte], out: DataOutput): Unit = {
    var i = 0
    while (i < arr.length) {
      out.writeByte(arr(i))
      i += 1
    }
  }

  def writeShort(arr: Array[Short], out: DataOutput): Unit = {
    var i = 0
    while (i < arr.length) {
      out.writeShort(arr(i))
      i += 1
    }
  }

  def writeInt(arr: Array[Int], out: DataOutput): Unit = {
    var i = 0
    while (i < arr.length) {
      out.writeInt(arr(i))
      i += 1
    }
  }

  def writeLong(arr: Array[Long], out: DataOutput): Unit = {
    var i = 0
    while (i < arr.length) {
      out.writeLong(arr(i))
      i += 1
    }
  }

  def writeFloat(arr: Array[Float], out: DataOutput): Unit = {
    var i = 0
    while (i < arr.length) {
      out.writeFloat(arr(i))
      i += 1
    }
  }

  def writeDouble(arr: Array[Double], out: DataOutput): Unit = {
    var i = 0
    while (i < arr.length) {
      out.writeDouble(arr(i))
      i += 1
    }
  }

  def writeChar(arr: Array[Char], out: DataOutput): Unit = {
    var i = 0
    while (i < arr.length) {
      out.writeChar(arr(i))
      i += 1
    }
  }

  def writeString(arr: Array[String], out: DataOutput): Unit = {
    var i = 0
    while (i < arr.length) {
      out.writeUTF(arr(i))
      i += 1
    }
  }
}

import DenseArrayLayout._

case object DenseBooleanLayout extends DenseArrayLayout[Boolean](DenseBooleanVec, readBoolean, writeBoolean)
case object DenseByteLayout extends DenseArrayLayout[Byte](DenseByteVec, readByte, writeByte)
case object DenseShortLayout extends DenseArrayLayout[Short](DenseShortVec, readShort, writeShort)
case object DenseIntLayout extends DenseArrayLayout[Int](DenseIntVec, readInt, writeInt)
case object DenseLongLayout extends DenseArrayLayout[Long](DenseLongVec, readLong, writeLong)
case object DenseFloatLayout extends DenseArrayLayout[Float](DenseFloatVec, readFloat, writeFloat)
case object DenseDoubleLayout extends DenseArrayLayout[Double](DenseDoubleVec, readDouble, writeDouble)
case object DenseCharLayout extends DenseArrayLayout[Char](DenseCharVec, readChar, writeChar)
case object DenseStringLayout extends DenseArrayLayout[String](DenseStringVec, readString, writeString)

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

  override def inflate(implicit ct: ClassTag[A]): Vec[A] =
    this
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

case class DenseStringVec(vec: Array[String]) extends DenseVec[String] {
  def apply(index: Int): String = vec(index)
}

case object UnitLayout extends Layout[Unit] {
  def newBuilder: DenseUnitBuilder =
    new DenseUnitBuilder

  def write(vec: Vec[Unit], out: DataOutput): Unit =
    out.writeInt(vec.size)

  def read(in: DataInput): Vec[Unit] =
    DenseUnitVec(in.readInt())

  def isSafeToCast(vec: Vec[_]): Boolean = vec match {
    case DenseUnitVec(_) => true
    case _ => false
  }
}

class DenseUnitBuilder extends VecBuilder[Unit] {
  var len = 0
  def +=(u: Unit) = {
    len += 1
    this
  }
  def clear(): Unit = { len = 0 }
  def result(): Vec[Unit] = DenseUnitVec(len)
}

case class DenseUnitVec(size: Int) extends Vec[Unit] {
  def apply(index: Int): Unit =
    if (index >= 0 && index < size) {
      ()
    } else {
      throw new IndexOutOfBoundsException(index.toString)
    }
}
