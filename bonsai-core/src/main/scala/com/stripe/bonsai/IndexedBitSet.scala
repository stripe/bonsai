package com.stripe.bonsai

import scala.annotation.switch
import scala.collection.immutable.BitSet
import scala.collection.mutable.{ Builder, ArrayBuilder }

/**
 * IndexedBitSet is an immutable set for storing non-negative integer
 * values.
 * 
 * This data structure stores non-negative integers. It has O(1)
 * lookup and rank operations. It also supports O(polylog n) select
 * operations, which are not currently implemented.
 *
 * To support fast rank, this bitset requires ~1.37n bits, rather than
 * the n bits that other bitsets (e.g. `scala.BitSet`) would use.
 */
final class IndexedBitSet(
  val bits: Array[Int],
  val length: Int,
  val level2Start: Int,
  val rawBitsStart: Int
) {
  import IndexedBitSet.rankWord

  /**
   * Test if `i` is contained in this bitset.
   *
   * The value `i` is expected to be non-negative.
   */
  def apply(i: Int): Boolean = {
    val wordOffset = rawBitsStart + (i >>> 5)
    val word = bits(wordOffset)
    val bitOffset = i & 0x1F
    ((word >>> bitOffset) & 1) != 0
  }

  /**
   * Return the number of 1 bits at-or-below `i`.
   *
   * For example, given `IndexedBitSet("110110")`, `.rank(0)` would
   * return `1` and `.rank(3)` would return `3`.
   *
   * The value `i` is expected to be non-negative.
   */
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

  /**
   * Iterate over the Boolean values contained in the bitset.
   *
   * The first Boolean value corresponds to 0, the second to 1, and so
   * on, up to the length of the bitset (after which all values are
   * false).
   */
  def iterator: Iterator[Boolean] =
    (0 until length).iterator.map(apply)

  /**
   * Convert this bitset to a scala.Bitset value.
   */
  def toBitSet: BitSet = {
    val b = BitSet.newBuilder
    var i = 0
    val it = iterator
    while (it.hasNext) {
      if (it.next) b += i
      i += 1
    }
    b.result
  }

  /**
   * Produce a string representation of this bitset.
   *
   * The string `s` produced here can be used to reconstruct the
   * original bitset via `BitSet(s)`.
   */
  override def toString: String =
    iterator
      .map(b => if (b) '1' else '0')
      .mkString("IndexedBitSet(", "", ")")
}

object IndexedBitSet {

  /**
   * IndexedBitSet containing no values.
   *
   * Since the bitset is immutable, only one empty value is necessary.
   */
  val empty: IndexedBitSet = new IndexedBitSet(new Array[Int](0), 0, 0, 0)

  /**
   * Create a new IndexedSetBuilder.
   *
   * This is the best way to createa new IndexedBitSet. See
   * IndexedBitSetBuilder for more information.
   */
  def newBuilder: IndexedBitSetBuilder = new IndexedBitSetBuilder

  /**
   * Parse a string as a bitset.
   *
   * The string is expected to consist of 0 (false) and 1 (true)
   * values. However, any non-1 character will be interpreted as a 0
   * (false) value.
   */
  def apply(s: String): IndexedBitSet =
    if (s == "") {
      empty
    } else {
      fromIterator(s.iterator.map(_ == '1'))
    }

  /**
   * Construct an `IndexedBitSet` from a sequence of integers.
   *
   * The integers are expected to be non-negative.
   */
  def fromIterator(it: Iterator[Boolean]): IndexedBitSet =
    if (!it.hasNext) {
      empty
    } else {
      val bldr = newBuilder
      while (it.hasNext) { bldr += it.next }
      bldr.result()
    }

  /**
   * Construct an IndexedBitSet from a scala.BitSet.
   */
  def fromBitSet(bitSet: BitSet): IndexedBitSet =
    if (bitSet.isEmpty) {
      empty
    } else {
      val bldr = newBuilder
      (0 to bitSet.max).foreach { i => bldr += bitSet(i) }
      bldr.result()
    }

  /**
   * TODO: maybe tom can explain this?
   */
  def rankWord(word: Int, i: Int): Int = {
    val mask = ~(-1L << ((i + 1))).toInt
    java.lang.Integer.bitCount(word & mask)
  }

  /**
   * Divide `n` by `d`, rounding up when the result is not an integer.
   *
   * For example, calling `ceilDiv(16, 4)` would return `4` whereas
   * `ceilDiv(17, 4)` would return `5`.
   */
  def ceilDiv(n: Int, d: Int): Int =
    ((n.toLong + d - 1) / d).toInt
}

/**
 * This class is used to construct an IndexedBitSet.
 *
 * Unlike IndexedBitSet it is mutable. It's internal state consists of
 * the "in-progress" bitset.
 *
 * It expects to consume a sequence of true/false values (using the +=
 * method). The first value corresponds to 0, the second 1, the third
 * 2, and so on.
 *
 * For example:
 *
 *     val b = IndexedBitSet.newBuilder
 *     b += false
 *     b += true
 *     b += false
 *     b.result() // produces IndexedBitSet(010)
 *
 * If you need to reuse a builder, you must call `.clear()` first to
 * reset its internal state.
 */
class IndexedBitSetBuilder extends Builder[Boolean, IndexedBitSet] {

  import IndexedBitSet.ceilDiv

  var cnt = 0
  var k = 0
  var m = 0.toShort
  var head = 0
  val bits = ArrayBuilder.make[Int]()
  val level1 = ArrayBuilder.make[Int]()
  val level2 = ArrayBuilder.make[Short]()

  /**
   * Append another value to to the bitset.
   *
   * This method modifies the builder's internal state. It corresponds
   * to the membership status (true/false) of the "next" integer
   * value.  The caller is expected to be independently tracking this
   * information.
   */
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

  /**
   * Reset the builder's internal state.
   *
   * If you want to reuse a builder which has already been modified
   * with += or .result(), you will need to call this method first.
   */
  def clear() = {
    cnt = 0
    k = 0
    m = 0.toShort
    head = 0
    bits.clear()
    level1.clear()
    level2.clear()
  }

  /**
   * Finalize the builder, and return a bitset.
   *
   * This method modifies the internal state. Thus, it is NOT safe to
   * call this multiple times to construct the "same" bitset.
   *
   * After calling this method, the only safe method to call is
   * .clear().
   */
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

    new IndexedBitSet(bitString, cnt, rawLvl1.length, rawBitsStart)
  }
}
