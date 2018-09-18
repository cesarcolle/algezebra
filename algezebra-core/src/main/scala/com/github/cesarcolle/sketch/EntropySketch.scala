package com.github.cesarcolle.sketch

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

class EntropySketchMonoid[A : CMSHasher](k : Int, seed : Long) extends Monoid[ES[A]]{

  override def zero: ES[A] = ???

  override def plus(x: ES[A], y: ES[A]): ES[A] = ???
}



case class EntropySketchParams[A]() {


}

sealed abstract class ES[A] {}

case class ESItem[A](item : A, params : EntropySketchParams[A]) extends ES[A] {
}

case class ESZero[A](params : EntropySketchParams[A]) extends ES[A] {
}

case class ESInstances[A](countTable : CountsTable[A], params : EntropySketchParams[A]) extends ES[A] {
}



