package com.github.cesarcolle.sketch

import com.github.cesarcolle.sketch.ESInstances.Count1DTable
import com.twitter.algebird.CMSInstance.CountsTable
import com.twitter.algebird.{CMSHasher, Monoid}

/**
  * From the paper : http://proceedings.mlr.press/v31/clifford13a.pdf
  *
  * In information theory, entropy is a measure of the uncertainty associated with a random variable.
  * The term by itself in this context usually refers to the Shannon entropy, which quantifies, in the sense of an expected value,
  * the information contained in a message, usually in units such as bits.
  * Equivalently, the Shannon entropy is a measure
  * of the average information content one is missing when one does not know the value of the random variable
  *
  * Determine the entropy of a stream with :
  * O(eps**-2 log(T) ) randoms bits for space.
  * */

trait EntropyCounting {

  def width(eps : Double)

}

class EntropySketchMonoid[A : CMSHasher](k : Int, seed : Long) extends Monoid[ES[A]]{

  override def zero: ES[A] = ???

  override def plus(x: ES[A], y: ES[A]): ES[A] = ???
}



case class EntropySketchParams[A]() {
  val k : Int = 0
}

sealed abstract class ES[A] {
  def +(other : ES[A])
  def ++(other : ES[A])
  def entropy() : Double

}

case class ESItem[A](item : A, params : EntropySketchParams[A]) extends ES[A] {
  override def +(other: ES[A]): Unit = ???

  override def ++(other: ES[A]): Unit = ???

  override def entropy(): Double = ???
}

case class ESZero[A](params : EntropySketchParams[A]) extends ES[A] {
  override def +(other: ES[A]): Unit = ???

  override def ++(other: ES[A]): Unit = ???

  override def entropy(): Double = ???
}


case class ESInstances[A](params : EntropySketchParams[A], countable : Count1DTable[A]) extends ES[A] {
  override def +(other: ES[A]): Unit = ???

  override def ++(other: ES[A]): Unit = ???

  override def entropy(): Double = ???
}

object ESInstances {
  def apply[A](params : EntropySketchParams[A]): ESInstances[A] ={
    ESInstances(params, Count1DTable(params.k))
  }


  case class Count1DTable[A](table: Vector[Long]) {

    def size = table.size

    def getCount(index : Int) : Long = ???

    def +(index : Int, count : Long) : Count1DTable[A] = {
      val oldCOunt = table(index)
      Count1DTable(table.updated(index, oldCOunt + count))
    }
    def ++(other : Count1DTable[A]): Count1DTable[A] = {
      require(size == other.size, "can't merge two differentes countTable")
      val newTable = table.zipWithIndex.map(f => f._1 + other.getCount(f._2))
      Count1DTable[A](newTable)
    }

  }
  object Count1DTable{
    def apply[A](size : Int): Count1DTable[A] = Count1DTable[A](Vector.fill[Long](size)(0L))
  }

}


