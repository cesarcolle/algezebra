package com.github.cesarcolle.clustering

import cats.kernel.Monoid


/**
  * Birch algorithm.
  *
  * Clustering algorithm for large data set.
  * paper : http://www.cs.uoi.gr/~pitoura/courses/dm07/birch.pdf
  *
  *
  * */

class Birch {

}

sealed trait BTH[A] {
  def +(item : A) : BTH[A]

  def ++ (other : BTH[A]) : BTH[A]

  def cluster(item : A) : Int
}


class BirchMonoid[A] extends Monoid[A] {
  override def empty: A = ???

  override def combine(x: A, y: A): A = ???
}

class BTHZero[A] extends BTH[A] {
  override def +(item: A): BTH[A] = ???

  override def ++(other: BTH[A]): BTH[A] = ???

  override def cluster(item: A): Int = ???
}

class BTHItem[A] extends BTH[A] {
  override def +(item: A): BTH[A] = ???

  override def ++(other: BTH[A]): BTH[A] = ???

  override def cluster(item: A): Int = ???
}

class BTHInstances[A] extends BTH[A] {
  override def +(item: A): BTH[A] = ???

  override def ++(other: BTH[A]): BTH[A] = ???

  override def cluster(item: A): Int = ???
}