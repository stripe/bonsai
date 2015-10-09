package com.stripe.bonsai

import scala.annotation.switch
import scala.collection.immutable.BitSet
import scala.collection.mutable.{ Builder, ArrayBuilder }

/**
 * A data structure for storing bitsets, with O(1) rank and O(polylog n) select
 * operations. To support fast rank queries, this bitset requires ~1.37n bits,
 * rather than the n bits that, eg, Scala's `BitSet` uses.
 */
final class Bitset(
  val bits: Array[Int],
  val length: Int,
  val level2Start: Int,
  val rawBitsStart: Int
) {
  import Bitset.{ ceilDiv, rankWord }

  def apply(i: Int): Boolean = {
    val wordOffset = rawBitsStart + (i >>> 5)
    val bitOffset = i & 0x1F
    val word = bits(wordOffset)
    ((word >>> bitOffset) & 1) != 0
  }

  def toBitSet: BitSet =
    BitSet(toSeq.zipWithIndex.collect { case (true, n) => n }: _*)

  def rank(i: Int): Int = {
    if (i >= length) {
      rank(length - 1)
    } else {
      val rank1 = bits(i >>> 10)

      val rank2Index = i >>> 5
      val rank2Word = bits(level2Start + (rank2Index / 3))
      val rank2 = (rank2Word >>> (10 * (rank2Index % 3))) & 0x3FF

      val rank3Word = bits(rawBitsStart + (i >>> 5))
      val rank3Offset = i & 0x1F
      val rank3 = rankWord(rank3Word, rank3Offset)

      rank1 + rank2 + rank3
    }
  }

  def toSeq: Seq[Boolean] =
    (0 until length)
      .map { i =>
        val word = bits(rawBitsStart + (i >>> 5))
        val offset = i & 0x1F
        val bit = (word >>> offset) & 1
        bit != 0
      }

  override def toString: String =
    toSeq
      .map { x => if (x) "0" else "1" }
      .reverse
      .mkString("Bitset(", "", ")")
}

object Bitset {
  def newBuilder: BitsetBuilder = new BitsetBuilder

  val empty: Bitset = new Bitset(new Array[Int](0), 0, 0, 0)

  def fromBitSet(bitSet: BitSet): Bitset = {
    if (bitSet.isEmpty) {
      empty
    } else {
      val bldr = newBuilder
      (0 to bitSet.max).foreach { i =>
        bldr += bitSet(i)
      }
      bldr.result()
    }
  }

  def rankWord(word: Int, i: Int): Int = {
    val mask = if (i == 31) -1 else ~(Int.MinValue >> (30 - i))
    java.lang.Integer.bitCount(word & mask)
  }

  def ceilDiv(n: Int, d: Int): Int =
    ((n.toLong + d - 1) / d).toInt
}

class BitsetBuilder extends Builder[Boolean, Bitset] {
  import Bitset.ceilDiv

  var cnt = 0
  var k = 0
  var m = 0.toShort
  var head = 0
  val bits = ArrayBuilder.make[Int]()
  val level1 = ArrayBuilder.make[Int]()
  val level2 = ArrayBuilder.make[Short]()

  def clear() = {
    cnt = 0
    k = 0
    m = 0.toShort
    head = 0
    bits.clear()
    level1.clear()
    level2.clear()
  }

  def result() = {
    // We first "finalize" the builder, if need be. We *probably* shouldn't be
    // modifying the state here, but... yolo.
    if ((cnt % 32) != 0) {
      bits += head
    }

    val rawLvl1 = level1.result()
    val rawLvl2 = level2.result()
    val rawBits = bits.result()
    val len = rawLvl1.length + ceilDiv(rawLvl2.length, 3) + rawBits.length

    // The final bit string.
    val bitString = new Array[Int](len)

    // Copy level 1 dictionary.
    Array.copy(rawLvl1, 0, bitString, 0, rawLvl1.length)

    // Copy level 2 dictionary.
    var i = rawLvl1.length
    var j = 0
    while (j < rawLvl2.length) {
      // We store a value between [0, 1024), so we need 10 bits. This means we
      // can stuff in 3 values per Int (if we're aligning on Int boundaries).
      val wordOffset = 10 * (j % 3)
      bitString(i) |= (rawLvl2(j).toInt << wordOffset)
      if (j % 3 == 2) {
        i += 1
      }
      j += 1
    }

    // Copy final ("level 3") raw bits.
    val rawBitsStart = if (j % 3 != 0) i + 1 else i
    Array.copy(rawBits, 0, bitString, rawBitsStart, rawBits.length)

    new Bitset(bitString, cnt, rawLvl1.length, rawBitsStart)
  }

  def +=(x: Boolean) = {
    if (cnt % 1024 == 0) {
      m = 0
      level1 += k
    }
    val headPos = cnt % 32
    if (headPos == 0) {
      level2 += m
    }
    if (x) {
      m = (m + 1).toShort
      k += 1
      head = head | (1 << (headPos))
    }
    cnt += 1
    if (cnt % 32 == 0) {
      bits += head
      head = 0
    }
    this
  }
}
