package com.github.cesarcolle


import com.googlecode.javaewah.{EWAHCompressedBitmap => CBitSet}

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}

object CuckooTestUtils {

  def toInstances[A](cf: CF[A]): CFInstance[A] = cf match {
    case CFZero(hash, fp, bck)             => CFInstance[A](hash, fp, bck)
    case CFItem(hash, fp, bck, it)         => CFInstance.apply[A](hash, fp, bck) + it
    case instance @ CFInstance(_, _, _, _) => instance
  }
}


class CuckooFilterTest extends WordSpec with Matchers {

  val RAND = new scala.util.Random

  "a cuckoo item " should {
    "be ++ with other CFItem" in {
      val a = new CFItem[String](new CFHash[String](255), 5, 255, "Aline")
      val b = new CFItem[String](new CFHash[String](255), 5, 255, "pour qu'elle revienne")
      val c = a ++ b
      assert(c.isInstanceOf[CFInstance[String]])
      assert(c.lookup("Aline") && c.lookup("pour qu'elle revienne"))
    }

  }

  "a cuckoo filter " should {

    // 255 buckets by default
    "be constructed from an iterator" in {
      val cfMonoid = new CuckooFilterMonoid[String](RAND.nextInt(10) + 1)

      val entries = (0 until 10).map(_ => RAND.nextInt.toString)
      val monoid = cfMonoid.create(entries.iterator)
      assert(monoid.isInstanceOf[CF[String]])
    }

    // 255 buckets by default
    "be constructed from an seq" in {
      val cfMonoid = new CuckooFilterMonoid[String](RAND.nextInt(10) + 1)

      val entries = (0 until 10).map(_ => RAND.nextInt.toString)
      val monoid = cfMonoid.create(entries: _*)
      assert(monoid.isInstanceOf[CF[String]])
    }

    "Add a fingerprint to the filter" in {
      val bs = Array.fill[CBitSet](255)(new CBitSet(64 * 4))

      val cuckooTest = CFInstance[String](new CFHash[String](255), bs, 1, 255)
      val hashedValue = 75
      var cuckoo = cuckooTest + "item-1"

      assert(cuckoo.cuckooBitSet(hashedValue).cardinality() == 1)

    }
    "Add to other bucket if first bucket is full" in {
      val bs = Array.fill[CBitSet](255)(new CBitSet(8 * 8 + 8))

      var cuckooTest = CFInstance[String](new CFHash[String](255), bs, 1, 255)
      val secondHasValue = 184
      cuckooTest = cuckooTest + "item-1"
      cuckooTest = cuckooTest + "item-1"
      assert(cuckooTest.cuckooBitSet(secondHasValue).cardinality() == 1)
    }

    "Kick a element if buckets are full" in {
      val bs = Array.fill[CBitSet](255)(new CBitSet(8 * 8 + 8))

      var cuckooTest = CFInstance[String](new CFHash[String](255), bs, 1, 255)
      cuckooTest = cuckooTest + "item-10"
      cuckooTest = cuckooTest + "item-10"
      cuckooTest = cuckooTest + "item-10"
      assert(cuckooTest.cuckooBitSet.map(_.cardinality()).sum == 3)
    }

    "work as an Aggregator" in {
      (0 to 10).foreach { _ =>
      {
        val aggregator = CuckooFilterAggregator[String](RAND.nextInt(5) + 1, RAND.nextInt(64) + 32)
        val numEntries = 5
        val entries = (0 until numEntries).map(_ => RAND.nextInt.toString)
        val bf = aggregator(entries)

        entries.foreach { i =>
          assert(bf.lookup(i))
        }
      }
      }
    }

    "be used like a bloomfilter" in {
      val bfMonoid1 = new CuckooFilterMonoid[String](32, 256)
      val bf1 = bfMonoid1.create("1", "2", "3", "4", "100")
      val lookup = bf1.lookup("1")
      assert(lookup)

    }
  }
}
