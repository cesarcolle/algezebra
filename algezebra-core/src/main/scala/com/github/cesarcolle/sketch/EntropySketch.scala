package com.github.cesarcolle.sketch

import com.github.cesarcolle.sketch.ESInstances.Count1DTable
import com.twitter.algebird.{CMSHash, CMSHasher, Monoid}

import scala.util.Random

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
  *
  * We are interest here to maintain entropy value.
  **/

trait EntropyCounting {

  def width(eps: Double)

}

class EntropySketchMonoid[A: CMSHasher](k: Int, seed: Long) extends Monoid[ES[A]] {

  override def zero: ES[A] = ???

  override def plus(x: ES[A], y: ES[A]): ES[A] = ???
}


case class EntropySketchParams[K: CMSHasher]() {
  val k: Int = 0
  val a: Int = Random.nextInt().abs
  val b: Int = Random.nextInt().abs

  def hash(): CMSHash[K] =
    CMSHash[K](a, b, Int.MaxValue)

}

sealed abstract class ES[A] {
  def +(item: A, count: Long): ES[A]

  def ++(other: ES[A])

  def entropy(): Double

}

case class ESZero[A](params: EntropySketchParams[A]) extends ES[A] {

  override def ++(other: ES[A]): Unit = other

  override def entropy(): Double = 0.0

  override def +(item: A, count: Long): ES[A] = ESItem[A](item, count, params)
}


case class ESItem[A](item: A, count: Long, params: EntropySketchParams[A]) extends ES[A] {

  override def ++(other: ES[A]): Unit = other match {
    case ESZero(_) =>
    case other@ESItem(_, _, _) => this + (other.item, other.count)
    case other@ESInstances(_, _, _) => other + (item, count)
  }

  override def entropy(): Double = -(Math.log(count) / Math.log(2.0))

  override def +(item: A, count: Long): ES[A] = ESInstances[A](params) + (this.item, this.count) + (item, count)
}

case class ESInstances[A](params: EntropySketchParams[A], count: Long, countTable: Count1DTable[A]) extends ES[A] {

  override def ++(other: ES[A]): Unit = other match {
    case ESZero(_) => this
    case ESItem(it, c, _) => this + (it, c)
    case other@ESInstances(prms, cnt, cTable) =>
      require(prms == this.params)
      ESInstances(params, cnt + count, countTable ++ cTable)
  }

  /**
    *Estimate the entropy of the stream.
    */

  override def entropy(): Double = {
    val sum = countTable.table.map(d => Math.exp(d / count)).sum
    -(Math.log(sum / params.k) / Math.log(2))
  }

  /**
    * Add item to the sketch.
    **/
  override def +(item: A, count: Long): ES[A] = {
    val it = params.hash().apply(item)
    val newTable =
      (0 until params.k).foldLeft(countTable) {
        (table, j) =>
          val skew = maxSkew()
          table + (j, skew * count)
      }
    new ESInstances[A](params, this.count + count, newTable)
  }

  /**
    * from : http://proceedings.mlr.press/v31/clifford13a.pdf p 198 (or p 3 of the pdf file)
    * Table 1 : Algorithm to simulate from the maximally skewed stable distribution F(x; 1, -1, PI / 2 , 0)
    **/
  private def maxSkew(): Double = {

    val r1 = Random.nextDouble()
    val r2 = Random.nextDouble()

    val w1 = Math.PI * (r1 * 0.5)
    val w2 = Math.PI * (r2 * 0.5)

    val halfPiW1 = Math.PI / 2 - w1

    Math.tan(w1) * halfPiW1 + (Math.log(w2 * (Math.cos(w1) / halfPiW1)) / Math.log(2))
  }
}

object ESInstances {
  def apply[A](params: EntropySketchParams[A]): ESInstances[A] = {
    ESInstances(params, 0, Count1DTable(params.k))
  }


  case class Count1DTable[A](table: Vector[Double]) {

    def size = table.size

    def getCount(index: Int): Double = table(index)

    def +(index: Int, count: Double): Count1DTable[A] = {
      val oldCount = table(index)
      Count1DTable(table.updated(index, oldCount + count))
    }

    def ++(other: Count1DTable[A]): Count1DTable[A] = {
      require(size == other.size, "can't merge two differentes sized countTable")
      val newTable = table.zipWithIndex.map(f => f._1 + other.getCount(f._2))
      Count1DTable[A](newTable)
    }
  }

  object Count1DTable {
    def apply[A](size: Int): Count1DTable[A] = Count1DTable[A](Vector.fill[Double](size)(0L))
  }

}


