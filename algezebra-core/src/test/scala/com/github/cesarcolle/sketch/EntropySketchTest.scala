package com.github.cesarcolle.sketch

import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

class EntropySketch extends WordSpec with Matchers{

  "an entropy sketch " should {

    "add an item " in {

    }
  }

}


class EntropySketchItem extends WordSpec with Matchers {
  val params: EntropySketchParams[String] = EntropySketchParams[String](0.6)
  "an EntropySketchItem " should  {
    "return proper ESInstances when add an item " in {
      val esItem = ESItem[String]("item-1", 1, params)
      val is = esItem + ("item-2", 14)
      assert(is.isInstanceOf[ESInstances[String]])
      assert(is.count == 15)
    }

    "return proper entropy value " in {
      val esItem = ESItem[String]("item-1", 10, params)
      val entropy =  -(Math.log(10) / Math.log(2.0))
      assert(esItem.entropy() == entropy)
    }

  }
}


class EntropySketchInstances extends WordSpec with  Matchers {
  val params: EntropySketchParams[Int] = EntropySketchParams[Int](0.99)
  val paramsStr: EntropySketchParams[String] = EntropySketchParams[String](0.9)

  "an ESInstances " should {
    "increment is count with + " in {
      val is = ESInstances[String](paramsStr)
      var test = is + ("item-1", 10)
      assert(test.count == 10)

      val test2 = test + ("item-2", 10)
      assert(test2.count == 20)
    }
    "return proper estimation entropy " in {
      val data = Array(10, 20, 30, 40, 50, 60, 70, 80)
      val zero = ESInstances[Int](params)

      println("create instances ...")
      val is = data.foldLeft(zero) {
        (es, j) => (es + (j, 1)).asInstanceOf[ESInstances[Int]]
      }
      println("create true entropy")
      val exactSize = data.sum
      val exactEntropy = data.map(_ * Math.log(2)).sum

      val exact = Math.log(exactSize)/ math.log(2) - exactEntropy/exactSize

      assert(exact >= is.entropy())
    }
  }


}


