package com.github.cesarcolle.clustering

case class CFNode(maxEntries : Int, distThreshold : Double,
                  distFunction : Int,
                  merging : Boolean,
                  leafStatus : Boolean,
                  entries: Vector[CFEntry] = Vector.empty[CFEntry]) {




}

object CFEntry {
  def apply(): CFEntry = new CFEntry(0, Vector.empty[Double], Vector.empty[Double])

  def apply(n: Int, sumX: Vector[Double], sumX2: Vector[Double]): CFEntry = new CFEntry(n, sumX, sumX2)

  def apply(index: Int, data: Vector[Double]): CFEntry = {
    val sumX2 = data.map(d => d * d)
    val indexes = Vector(index)
    new CFEntry(0, data, sumX2, Some(indexes))
  }

}

case class CFEntry(n: Int, sumX: Vector[Double], sumX2: Vector[Double],
                   index: Option[Vector[Int]] = None,
                   child: Option[CFNode] = None) {

  def update(cFEntry: CFEntry): CFEntry = {
    val newCount = n + cFEntry.n
    val newSumX: Vector[Double] = sumX.zip(cFEntry.sumX).map(b => b._1 + b._2)
    val newSumX2: Vector[Double] = sumX2.zip(cFEntry.sumX2).map(b => b._1 + b._2)

    child match {
      case None => CFEntry(newCount, newSumX, newSumX2)
      case _ =>
        val newIndex = index
          .flatMap { idx =>
            cFEntry.index.map { idxExternal => idx ++ idxExternal }
              .orElse(cFEntry.index)
          }
        CFEntry(newCount, newSumX, newSumX2, newIndex)
    }

  }

  def addToChild(cFEntry: CFEntry): CFEntry = {
    val newChild = child.fold(Vector.empty[CFEntry])(node =>
      node.entries ++ cFEntry.child.fold(Vector.empty[CFEntry])(_.entries))
    CFEntry()
  }

}


class CFTree {

}
