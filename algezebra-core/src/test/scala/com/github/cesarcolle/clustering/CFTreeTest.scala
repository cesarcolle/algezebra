package com.github.cesarcolle.clustering

import org.scalatest.WordSpec

class CFTreeTest extends WordSpec {


}

class CFNodeTest extends WordSpec {

  val data = Array(1.0, 2.0, 3.0, 4.0)

  "a cfNode " should {
    "insert properly a CFENTRY " in {
      val rootNode = CFNode(100, 0.5, DistOne, true, false)
      println(rootNode.insertEntry(CFEntry(data.size ,data.toVector)))
    }

  }

}