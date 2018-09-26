package com.github.cesarcolle.clustering


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



case class CFTree() {



}

sealed trait BTH[A] {
  def +(item : A) : BTH[A]

  def ++ (other : BTH[A]) : BTH[A]

  def cluster(item : A) : Int

}
