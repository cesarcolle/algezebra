package com.github.cesarcolle.algezebra
package benchmark

import com.github.cesarcolle.algezebra.benchmark.CuckooFilterBenchmarkQuery.CuckooFilterState
import com.github.cesarcolle.filter._
import com.twitter.algebird.{BF, BloomFilter}
import org.openjdk.jmh.annotations._

object CuckooFilterBenchmarkQuery {

  @State(Scope.Benchmark)
  class CuckooFilterState {

    @Param(Array("100", "1000", "10000"))
    var nbrOfElements: Int = 0

    @Param(Array("100", "300"))
    var bucketNumber: Int = 0

    @Param(Array("10", "50"))
    var fingerprintBucket: Int = 0

    var cf: CF[String] = _

    var bf : BF[String] = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      val randomStrings =
        CuckooFilterCreateBenchmark.createRandomString(nbrOfElements, 10)
      cf = CuckooFilter[String](fingerprintBucket, bucketNumber)
        .create(randomStrings: _*)

      bf = BloomFilter[String](bucketNumber * fingerprintBucket, 0.99)
        .create(randomStrings :_*)

    }
  }
}

class CuckooFilterBenchmarkQuery {

  @Benchmark
  def queryCuckooFilter(cuckooFilterState: CuckooFilterState): Boolean =
    cuckooFilterState.cf.lookup("1")

  @Benchmark
  def queryBloomFilter(cuckooFilterState: CuckooFilterState) : Boolean =
    cuckooFilterState.bf.maybeContains("1")
}
