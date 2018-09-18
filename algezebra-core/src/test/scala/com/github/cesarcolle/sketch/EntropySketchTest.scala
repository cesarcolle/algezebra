package com.github.cesarcolle.sketch

import org.scalatest.{Matchers, WordSpec}

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
  }
}


class EntropySketchInstances extends WordSpec with  Matchers {
  val params: EntropySketchParams[String] = EntropySketchParams[String](0.6)

  "an ESInstances " should {
    "increment is count with + " in {
      val is = ESInstances[String](params)
      var test = is + ("item-1", 10)
      assert(test.count == 10)

      var test2 = test + ("item-2", 10)

      assert(test2.count == 20)
    }

  }


}
