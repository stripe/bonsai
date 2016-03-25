package com.stripe.bonsai

import scala.annotation.switch
import scala.collection.immutable.BitSet
import scala.collection.mutable.{ Builder, ArrayBuilder }

/**
 * IndexedBitSet is an immutable set for storing non-negative integer values.
 *
 * This data structure stores non-negative integers. It has O(1) lookup and
 * rank operations. It also supports O(polylog n) select operations.
 *
 * To support fast rank and select, this bitset requires ~1.37n bits, rather
 * than the n bits that other bitsets (e.g. `scala.BitSet`) would use.
 *
 * Unlike some immutable structures, there is no form of structural sharing
 * here. Thus, the only way to "modify" one of these structures is to build a
 * new one that has the desired modifications (which will not be efficient if
 * done frequently).
 */
final class IndexedBitSet(
  val bits: Array[Int],
  val length: Int,
  val level2Start: Int,
  val rawBitsStart: Int
) {
  import IndexedBitSet.{rankWord, selectWord, ceilDiv}

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

  // Returns the 10-bit block-relative rank stored in the level 2 "index".
  // We pack in 3 ranks per 32-bit word, so getting them is a bit complicated.
  private def getLevel2(i: Int): Int = {
    val word = bits(level2Start + (i / 3))
    (word >>> (10 * (i % 3))) & 0x3FF
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
      rank1(i) + rank2(i) + rank3(i)
    }
  }

  // Returns the initial rank of the block storing bit i.
  private def rank1(i: Int): Int = {
    bits(i >>> 10)
  }

  // Returns the block-relative rank of the word storing bit i.
  private def rank2(i: Int): Int = {
    getLevel2(i >>> 5)
  }

  // Returns the word-relative rank of bit i.
  private def rank3(i: Int): Int = {
    val rank3Word = bits(rawBitsStart + (i >>> 5))
    val rank3Offset = i & 0x1F
    rankWord(rank3Word, rank3Offset)
  }

  /**
   * Returns the index of the `i`-th set bit in this bitset. This has the
   * property that `bitset.rank(bitset.select(i)) = i` and
   * `bitset(bitset.select(i)) == true`.
   *
   * @param i an integer in `[1, bitset.bitCount]`
   * @returns an integer
   */
  def select(i: Int): Int = {
    val blockOffset = search1(i)
    val wordOffset = search2(blockOffset, i)
    val bitOffset = search3(blockOffset, wordOffset, i)
    32 * wordOffset + bitOffset
  }

  // Find the index in level1 where i would be located. This will return a
  // value between [-1,level2Start), technically, but since bits(0) is always
  // 0, this should return values between [0, level2Start). The main difference
  // from Java's Arrays.binarySearch is that we don't return early if we find
  // a block rank equal to i, since, if there are multiple blocks with the rank
  // i, we need to ensure we return the last valid block. Arrays.binarySearch
  // makes no guarantees about which element will be chosen.
  private def search1(i: Int): Int = {
    var l: Int = 0
    var r: Int = level2Start - 1
    while (l <= r) {
      val c = (l + r) >>> 1
      val x = bits(c)
      if (x < i) {
        l = c + 1
      } else if (x >= i) {
        r = c - 1
      }
    }
    l - 1
  }

  // Returns the index into level 2 where the bit with the given rank is
  // located. Note that the index returned by this is relative to the start
  // of the level 2 index, NOT the block. Hence, the word that contains the
  // set bit with rank `rank` is `bits(rawBitsStart + search2(block, rank))`.
  // The rank should be the absolute rank in the bitset.
  private def search2(block: Int, rank: Int): Int = {
    val rank2 = rank - bits(block)
    var l: Int = 32 * block
    var r: Int = math.min(l + 32, ceilDiv(length, 32)) - 1
    while (l <= r) {
      val c = (l + r) >>> 1
      val x = getLevel2(c)
      if (x < rank2) {
        l = c + 1
      } else if (x >= rank2) {
        r = c - 1
      }
    }
    l - 1
  }

  // Returns index of the bit in the given word (given by wordOffset) with rank
  // `rank`. The rank should be the absolute rank in the bitset.
  private def search3(blockOffset: Int, wordOffset: Int, rank: Int): Int = {
    val rank3 = rank - bits(blockOffset) - getLevel2(wordOffset)
    val word = bits(rawBitsStart + wordOffset)
    selectWord(word, rank3)
  }

  /**
   * Returns the number of set 1 bits in this bitset.
   */
  def bitCount: Int =
    if (length > 0) rank(length - 1) else 0

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
    var i: Int = 0
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
   * Returns the rank of the i-th bit in a 32-bit word.
   */
  def rankWord(word: Int, i: Int): Int = {
    val mask = ~(-1L << ((i + 1))).toInt
    java.lang.Integer.bitCount(word & mask)
  }

  /**
   * Returns the index of the i-th set bit in a 32-bit word.
   *
   * @param word a 32-bit word to search
   * @param rank a value in [1,32] (inclusive)
   */
  def selectWord(word: Int, rank: Int): Int = {

    // This first part is basically the bitCount algorithm, however we keep a
    // copy of the intermediate states, which we'll use later.

    var sum1: Int  = word
    var sum2: Int  = (sum1  & 0x55555555) + ((sum1  >>>  1) & 0x55555555)
    var sum4: Int  = (sum2  & 0x33333333) + ((sum2  >>>  2) & 0x33333333)
    var sum8: Int  = (sum4  & 0x0F0F0F0F) + ((sum4  >>>  4) & 0x0F0F0F0F)
    val sum16: Int = (sum8  & 0x00FF00FF) + ((sum8  >>>  8) & 0x00FF00FF)
    val sum32: Int = (sum16 & 0x0000FFFF) + ((sum16 >>> 16) & 0x0000FFFF)

    // This shouldn't come up in IndexedBitSet, but might as well play it safe.
    if (sum32 < rank)
      throw new IllegalArgumentException("bit out of range")

    // We basically perform a binary search for the the index with the given
    // rank. We use the intermediate states of the bit count above to do this
    // quickly. Starting from the half-word counts, we can determine which half
    // contains the rank-th set bit. If its the lower half we don't have to do
    // anything, otherwise we basically shift everything over by 16 (8, 4, 2, 1)
    // bits and try again, but this time using only the lower 16 (8, 4, 2, 1)
    // bits. The variable i maintains a lowerbound on the index to be returned.
    // Once we've gone through all available bit widths (16, 8, 4, 2, 1) it'll
    // point to the rank-th set bit.

    var i: Int = 0
    var currRank: Int = rank
    if ((sum16 & 0x0000FFFF) < currRank) {
      i += 16
      currRank -= sum16 & 0x0000FFFF
      sum8 = sum8 >>> 16
      sum4 = sum4 >>> 16
      sum2 = sum2 >>> 16
      sum1 = sum1 >>> 16
    }
    if ((sum8 & 0x000000FF) < currRank) {
      i += 8
      currRank -= sum8 & 0x000000FF
      sum4 = sum4 >>> 8
      sum2 = sum2 >>> 8
      sum1 = sum1 >>> 8
    }
    if ((sum4 & 0x0000000F) < currRank) {
      i += 4
      currRank -= sum4 & 0x0000000F
      sum2 = sum2 >>> 4
      sum1 = sum1 >>> 4
    }
    if ((sum2 & 0x00000003) < currRank) {
      i += 2
      currRank -= sum2 & 0x00000003
      sum1 = sum1 >>> 2
    }
    if ((sum1 & 0x00000001) < currRank) {
      i += 1
    }

    i
  }

  /**
   * Divide `n` by `d`, rounding up when the result is not an integer.
   *
   * For example, calling `ceilDiv(16, 4)` would return `4` whereas
   * `ceilDiv(17, 4)` would return `5`.
   */
  private[bonsai] def ceilDiv(n: Int, d: Int): Int =
    ((n.toLong + d - 1) / d).toInt

  /**
   * Writes out the raw bits from an [[IndexedBitSet]] to a `DataOutput` as
   * bytes, LSB. This will write out `ceil(bitset.length / 8)` bytes.
   *
   * @param out    the output to write bytes to
   * @param bitset the bitset to write out
   */
  def write(out: java.io.DataOutput, bitset: IndexedBitSet): Unit = {
    val bits = bitset.bits
    val rawBitsStart = bitset.rawBitsStart
    def getByte(i: Int): Int = {
      val wordOffset = i >>> 2
      val bitOffset = (i & 3) << 3
      val word = bits(rawBitsStart + wordOffset)
      (word >>> bitOffset) & 0xFF
    }

    val byteLen = ceilDiv(bitset.length, 8)
    var i = 0
    while (i < byteLen) {
      out.writeByte(getByte(i))
      i += 1
    }
  }

  /**
   * Read in a IndexedBitSet of the given length from a `DataInput`. This
   * expects the bitset to be encoded in bytes, LSB. This will consume
   * ceil(length / 8) bytes from the `DataInput`.
   *
   * @param in     the input to read bytes from
   * @param length the length of the bitset to read, in bits
   */
  def read(in: java.io.DataInput, length: Int): IndexedBitSet = {
    val byteLen = ceilDiv(length, 8)
    val bldr = new IndexedBitSetBuilder
    var i = 0
    while (i < byteLen) {
      val byte = in.readByte()
      val byteLen = math.min(length - i * 8, 8)
      var j = 0
      while (j < byteLen) {
        bldr += (byte & (1 << j)) != 0
        j += 1
      }
      i += 1
    }
    bldr.result()
  }
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

  var cnt: Int = 0
  var k: Int = 0
  var m: Short = 0.toShort
  var head: Int = 0
  val bits: ArrayBuilder[Int] = ArrayBuilder.make[Int]()
  val level1: ArrayBuilder[Int] = ArrayBuilder.make[Int]()
  val level2: ArrayBuilder[Short] = ArrayBuilder.make[Short]()

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
    var i: Int = rawLvl1.length
    var j: Int = 0
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
