package com.stripe.bonsai
package benchmark

import org.openjdk.jmh.annotations.Benchmark

class IndexedBitSetBenchmark {
  @Benchmark
  def selectWord(): Int = {
    IndexedBitSet.selectWord(-1, 32)
  }
}
