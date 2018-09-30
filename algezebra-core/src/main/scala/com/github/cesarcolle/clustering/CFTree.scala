package com.github.cesarcolle.clustering

/**
  * CFtree for BIRCH algorithm.
  *
  * Trying to translate https://github.com/perdisci/jbirch
  * In scala. Finding where functional > object aspect.
  * Making some test on what's good to keep, what's good to change.
  * Don't blame the code quality. It's quite a mess. I know about it.
  *
  *
  * author : @CésarCollé
  * */

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

  def apply(child: CFNode) = new CFEntry(0, Vector.empty[Double], Vector.empty[Double], child = Option(child))

  def apply(n: Int, sumX: Vector[Double], sumX2: Vector[Double],
            index: Option[Vector[Int]] = None,
            child: Option[CFNode] = None): CFEntry = new CFEntry(n, sumX, sumX2, index, child)

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

  def setChild(child: CFNode) : CFEntry = {
    new CFEntry(n, sumX, sumX2, index, Some(child))
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

  def childSize() : Int = {
    child match {
      case None => Int.MaxValue
      case node => node.get.entries.size
    }
  }

}

case class CFEntryPair(e1: CFEntry, e2: CFEntry) {}

object CFNode {
  def apply(maxEtry: Int, distThrsld : Double, distfunc : DistFunction,
    mrg : Boolean, leafStatus : Boolean, entries : Vector[CFEntry]) : CFNode = {
    val node = CFNode(maxEtry, distThrsld, distfunc, mrg, leafStatus)
    node.entries = entries
    node
  }

}

case class CFNode(maxEntries: Int, distThreshold: Double,
                  distFunction: DistFunction,
                  merging: Boolean,
                  leafStatus: Boolean) {

  var entries: Vector[CFEntry] = Vector.empty[CFEntry]
  var nextLeaf: Option[CFNode] = None
  var previousLeaf: Option[CFNode] = None

  def setPrevious(newPrev : CFNode) : Unit = {
    previousLeaf = previousLeaf.map(_ => newPrev)
  }

  def setNext(newNext : CFNode) : Unit = {
    nextLeaf = nextLeaf.map(_ => newNext)
  }

  def isDummy: Boolean = (maxEntries eq 0) && (distThreshold eq 0) && (this.size eq 0) && (previousLeaf.isDefined || nextLeaf.isDefined)

  def size: Int = entries.size

  def findClosestEntry(entry: CFEntry): CFEntry = {
    entries.minBy(f => f.distance(entry, distFunction))
  }

  def insertEntry(cFEntry: CFEntry): Option[CFNode] = {
    if (entries.isEmpty) {
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
            val splitPair: CFEntryPair = splitEntry(closest)

            if(entries.size > maxEntries )
              None
            else{
              if (merging){

              }


            }


          case Some(entry) => // I split
            val index = entries.indexOf(closest)
            entries.updated(index, closest.update(cFEntry))
            return Option(this)
        }
    }
    None
  }

  def isLeaf(): Boolean = leafStatus

  def resetEntries(): Unit = {
    this.entries = Vector.empty[CFEntry]
  }

  // to be replaced.
  private def replaceClosestPairWithNewEntries(p: CFEntryPair, newE1: CFEntry, newE2: CFEntry): Unit = {
    entries.indices.foreach{
      idx =>
      if (entries(idx) == p.e1) {
        entries = entries.updated(idx, newE1)
      }
        else if (entries(idx) == p.e2) {
        entries = entries.updated(idx, newE2)
      }
    }
  }


  // this is not so functional.
  private def splitEntry(closest: CFEntry): CFEntryPair = {
    // we are sure here there is a child
    val oldNode: CFNode = closest.child.get
    val oldEntries = oldNode.entries
    val p = findFarthestEntryPair(oldEntries)

    val newNode1 = new CFNode(maxEntries, distThreshold, distFunction, merging, oldNode.isLeaf())
    var entry1 = CFEntry(child = newNode1)

    val newNode2 = new CFNode(maxEntries, distThreshold, distFunction, merging, oldNode.isLeaf())
    var entry2 = CFEntry(child = newNode2)



    if (oldNode.isLeaf()) {
      val previousNode = oldNode.previousLeaf
      val nextNode = oldNode.nextLeaf

      previousNode match {
        case None =>
        case Some(node) => node.nextLeaf = Some(newNode1)
      }
      nextNode match {
        case None =>
        case Some(node) => node.previousLeaf = Some(newNode2)
      }

      newNode1.previousLeaf = previousNode
      newNode1.nextLeaf = Some(newNode2)

      newNode2.previousLeaf = Some(newNode1)
      newNode2.nextLeaf = nextNode

    }

    val redistributed = redistributeEntries(oldEntries, p.get, entry1, entry2)
    entries =  entries.filter(entry => entry == closest ).+:(redistributed._1).+:(redistributed._2)

    CFEntryPair(redistributed._1, redistributed._2)
  }

  private def redistributeEntries(oldEntries: Vector[CFEntry], pair: CFEntryPair,
                                  newE1: CFEntry, newE2: CFEntry): (CFEntry, CFEntry) = {
    oldEntries.foldLeft((newE1, newE2)) {
      (previous, entry) =>
        val dist1 = pair.e1.distance(entry, distFunction)
        val dist2 = pair.e2.distance(entry, distFunction)

        if (dist1 < dist2) {
          return (previous._1.addToChild(entry).addToChild(entry), previous._2)
        }
        else {
          return (previous._1, previous._2.addToChild(entry).addToChild(entry))
        }
    }
  }

  private def redistributeEntries(oldEntry1: Vector[CFEntry], oldEntry2: Vector[CFEntry],
                                  closeEntry: CFEntryPair, newE1: CFEntry, newE2: CFEntry) = {

    val allOldEntries = oldEntry1 ++ oldEntry2
    allOldEntries.foldLeft((newE1, newE2)) {
      (previous, entry) => {
        // To be move
        val dist1 = closeEntry.e1.distance(entry, distFunction)
        val dist2 = closeEntry.e2.distance(entry, distFunction)

        if(dist1 <= dist2) {
          if (previous._1.childSize() < maxEntries)
            (previous._1.addToChild(entry).update(entry), previous._2)
          else {
            (previous._1, previous._2.addToChild(entry).update(entry))
          }
        }
        else if (dist1 < dist2) {
          if (previous._2.childSize() < maxEntries) (previous._1, previous._2.addToChild(entry).update(entry))
          else (previous._1.addToChild(entry).update(entry), previous._2)
        }
        else{
          previous
        }
      }
    }
  }

  def redistributeEntries(oldEntry1: Vector[CFEntry], oldEntry2 : Vector[CFEntry], newE : CFEntry) : CFEntry = {
    val allOldEntries = oldEntry1 ++ oldEntry2
    allOldEntries.foldLeft(newE) {
      (prev, next) => prev.addToChild(next).update(next)
    }
  }


  private def findFarthestEntryPair(entries: Vector[CFEntry]): Option[CFEntryPair] = {
    if (entries.size < 2) return None
    val (heads, tails) = entries.splitAt(2)

    val entriesMin = tails.foldLeft(List[(Vector[CFEntry], Double)]((heads, heads(0).distance(heads(1), distFunction)))) {
      (h, otherEnry) =>
        val lastHead = h.last._1.last
        h ++
          List[(Vector[CFEntry], Double)](Vector[CFEntry](lastHead, otherEnry), lastHead.distance(otherEnry, distFunction))
    }.maxBy(f => f._2)

    Some(CFEntryPair(entriesMin._1(0), entriesMin._1(1)))
  }


  private def mergingRefinement(entryPair : CFEntryPair) : Unit = {
    val nodeEntries = entries
    val pair = findFarthestEntryPair(nodeEntries)

    pair match {
      case None =>
      case Some(p) =>
        if (p == entryPair) return

        val oldNode1 = p.e1.child.get
        val oldNode2 = p.e2.child.get

        val oldNodeEntries1 = oldNode1.entries
        val oldNodeEntries2 = oldNode2.entries

        require(oldNode1.isLeaf() != oldNode2.isLeaf(), "Node at same level must have same leaf status!")

        if (oldNodeEntries1.size + oldNodeEntries2.size  > maxEntries) {
          var newEntry = CFEntry()


          val newNode1 = oldNode1
          newNode1.resetEntries()

          newEntry = newEntry.setChild(newNode1)


          var newEntry2 = CFEntry()
          val newNode2 = oldNode2
          newNode2.resetEntries()
          newEntry2 = newEntry2.setChild(newNode2)

          val newEntries = redistributeEntries(oldNodeEntries1, oldNodeEntries2, p, newEntry, newEntry2)
          replaceClosestPairWithNewEntries(p, newEntry, newEntry2)
        }

        else {
          var newEntry = CFEntry()
// 	CFNode newNode = new CFNode(maxNodeEntries,distThreshold,distFunction,applyMergingRefinement,oldNode1.isLeaf());
          val newNode = CFNode(maxEntries, distThreshold, distFunction, merging, oldNode1.isLeaf())
          newEntry = newEntry.setChild(newNode)

          newEntry = redistributeEntries(oldNodeEntries1, oldNodeEntries2, newEntry)

          if (oldNode1.isLeaf() && oldNode2.isLeaf()) {
            oldNode1.previousLeaf =  oldNode1.previousLeaf match {
              case None => None
              case Some(prevLeaf) =>
                prevLeaf.setNext(newNode)
                Some(prevLeaf)
            }
            oldNode1.nextLeaf =  oldNode1.nextLeaf match {
              case None => None
              case Some(next) =>
                next.setPrevious(newNode)
                Some(next)
            }
            // TODO: to be modify
            newNode.setPrevious(oldNode1.previousLeaf.get)
            newNode.setNext(oldNode1.nextLeaf.get)

            var dummy = new CFNode(0, 0, DistZero, false, true)


          }

        }
    }

  }


}


class CFTree {

}
