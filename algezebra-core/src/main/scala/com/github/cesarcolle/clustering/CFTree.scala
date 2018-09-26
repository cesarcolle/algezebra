package com.github.cesarcolle.clustering

sealed trait DistFunction {
  def distance(e1: CFEntry, e2: CFEntry): Double

  def add4Vectors(v1: Vector[Double], v2: Vector[Double], v3: Vector[Double], v4: Vector[Double]): Vector[(Double, Double)] = {
    (v1 zip v2 zip v3 zip v4)
      .map { case (((a, b), c), d) => (a, b, c, d) }
      .map(tuples => (tuples._1 + tuples._2, tuples._3 + tuples._4))
  }
}

object DistZero extends DistFunction {
  override def distance(e1: CFEntry, e2: CFEntry): Double = {
    require(e1.sumX.size == e2.sumX.size, "can't compute dist on different Entries ...")

    val dist = e1.sumX.zip(e2.sumX).map(sums => Math.pow(sums._1 / e1.n - sums._2 / e2.n, 2)).sum
    require(dist > 0, "dist0 can't be negative..")
    Math.sqrt(dist)
  }
}

object DistOne extends DistFunction {
  override def distance(e1: CFEntry, e2: CFEntry): Double = {
    require(e1.sumX.size == e2.sumX.size, "can't compute dist on different Entries ...")
    val dist = e1.sumX.zip(e2.sumX).map(sums => (sums._1 / e1.n - sums._2 / e2.n).abs).sum
    require(dist > 0, "distOne can't be negative")

    dist
  }
}

object DistTwo extends DistFunction {
  override def distance(e1: CFEntry, e2: CFEntry): Double = {
    require(e1.sumX.size == e2.sumX.size, "can't compute dist on different Entries ...")
    val n1 = e1.n
    val n2 = e1.n
    val flat = (e1.sumX zip e2.sumX zip e1.sumX2 zip e2.sumX2) map { case (((a, b), c), d) => (a, b, c, d) }

    val dist = flat.map(tuples => (n2 * tuples._3 - 2 * tuples._1 * tuples._2 + n1 * tuples._4) / (n1 * n2)).sum
    require(dist > 0, "distOne can't be negative")

    dist
  }
}

object DistThree extends DistFunction {
  override def distance(e1: CFEntry, e2: CFEntry): Double = {
    val n1 = e1.n
    val n2 = e1.n

    val flatX1 = add4Vectors(e1.sumX, e2.sumX, e1.sumX2, e2.sumX2)
    val dist = flatX1.map(totX => ((n1 + n2) * totX._2 - 2 * Math.pow(totX._1, 2) + (n1 + n2) * totX._2)
      / ((n1 + n2) * (n1 + n2 - 1))).sum
    require(dist > 0, "distOne can't be negative")

    dist
  }
}

object DistFour extends DistFunction {
  override def distance(e1: CFEntry, e2: CFEntry): Double = {
    val n1 = e1.n
    val n2 = e2.n
    val tots = add4Vectors(e1.sumX, e2.sumX, e1.sumX2, e2.sumX2)
    val dist = e1.sumX.zipWithIndex.map { e1Value =>
      val e1SumX = e1Value._1
      val idx = e1Value._2

      val diff1 = tots(idx)._2 - 2 * tots(idx)._1 * tots(idx)._1 / (n1 + n2) + (n1 + n2) *
        (tots(idx)._1 / n1 + n2) * (tots(idx)._1 / (n1 + n2))
      val diff2 = e1.sumX2(idx) - 2 * e1SumX / n1 + n1 * (e1SumX / n1) * (e1SumX / n1)
      val diff3 = e2.sumX2(idx) - 2 * Math.pow(e2.sumX(idx), 2) / n2 + n2 * Math.pow(e2.sumX(idx) / n2, 2)

      diff1 - diff2 - diff3
    }.sum
    require(dist > 0, "distOne can't be negative")
    Math.sqrt(dist)
  }
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
    val entriesChild = child.fold(Vector.empty[CFEntry])(node =>
      node.entries ++ cFEntry.child.fold(Vector.empty[CFEntry])(_.entries))
    val newChild = child.map(f => CFNode(f.maxEntries, f.distThreshold, f.distFunction, f.merging, f.leafStatus, entriesChild))
    new CFEntry(n, sumX, sumX2, index, newChild)
  }

  def isWithinThreshold(e: CFEntry, threshold: Double, distFunction: DistFunction): Boolean = {
    val dist = distance(e, distFunction)

    if (dist == 0 || dist <= threshold) // read the comments in function d0() about differences with implementation in R
      return true
    false
  }

  def distance(e: CFEntry, func: DistFunction): Double = {
    func.distance(this, e)
  }

}

case class CFEntryPair(e1: CFEntry, e2 : CFEntry) {}

object CFNode {

}

case class CFNode(maxEntries: Int, distThreshold: Double,
                  distFunction: DistFunction,
                  merging: Boolean,
                  leafStatus: Boolean,
                  entries: Vector[CFEntry] = Vector.empty[CFEntry]) {

  private var nextLeaf : Option[CFNode]= None
  private var previousLeaf : Option[CFNode]= None


  def size: Int = entries.size
  def isDummy : Boolean = (maxEntries eq 0) && (distThreshold eq 0) && (this.size eq 0) && (previousLeaf.isDefined || nextLeaf.isDefined)


  def findClosestEntry(entry : CFEntry) : CFEntry = {
    entries.minBy(f => f.distance(entry, distFunction))
  }

  def insertEntry(cFEntry: CFEntry) : Option[CFNode] = {
    if(entries.isEmpty) {
      return Option(CFNode(maxEntries, distThreshold, distFunction, merging, leafStatus, entries.+:(cFEntry)))
    }
    val closest = findClosestEntry(cFEntry)

    closest.child match {
      case None =>
        // NO CHILD !!

      case Some(entryChild) =>
        // I have a child !
        entryChild.insertEntry(cFEntry) match {
          case None => // Don't split !
            val splitPair : CFEntryPair = null

          case Some(entry) => // I split
            val index = entries.indexOf(closest)
            entries.updated(index, closest.update(cFEntry))
            return Option(this)
        }
    }
    None
  }

  private def splitEntry(closest: CFEntry) : CFEntryPair = {
    // we are sure here there is a child
    val oldNode : CFNode =closest.child.get
    val oldEntries = oldNode.entries
    null
  }

  private def findFarthestEntryPair(entries : Vector[CFEntry]) :  Option[CFEntryPair] = {
    if (entries.size < 2) return None
    val (heads, tails) = entries.splitAt(2)

    val entriesMin = tails.foldLeft( List[(Vector[CFEntry], Double)]( (heads, heads(0).distance(heads(1), distFunction)) )) {
      (h, otherEnry) =>
        val lastHead = h.last._1.last
        h ++
          List[(Vector[CFEntry], Double)](Vector[CFEntry](lastHead, otherEnry), lastHead.distance(otherEnry, distFunction))
    }.maxBy(f => f._2)

    Some(CFEntryPair(entriesMin._1(0), entriesMin._1(1)))
  }


}


class CFTree {

}
