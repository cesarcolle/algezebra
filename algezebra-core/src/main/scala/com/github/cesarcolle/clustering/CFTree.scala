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
  **/

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
                   child: Option[CFNode] = None,
                   subclusterID: Option[Int] = None) {

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
        CFEntry(newCount, newSumX, newSumX2, newIndex, None)
    }
  }

  def setChild(child: CFNode): CFEntry = {
    new CFEntry(n, sumX, sumX2, index, Some(child))
  }

  def addToChild(cFEntry: CFEntry): CFEntry = {
    val entriesChild = child.fold(Vector.empty[CFEntry])(node =>
      node.entries ++ cFEntry.child.fold(Vector.empty[CFEntry])(_.entries))

    val newChild = child.map(f => f.copy(entries = entriesChild))
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

  def childSize(): Int = {
    child match {
      case None => Int.MaxValue
      case node => node.get.entries.size
    }
  }

}

case class CFEntryPair(e1: CFEntry, e2: CFEntry) {}

object CFNode {
  def apply(maxEtry: Int, distThrsld: Double, distfunc: DistFunction,
            mrg: Boolean, leafStatus: Boolean, entries: Vector[CFEntry]): CFNode = {
    val node = CFNode(maxEtry, distThrsld, distfunc, mrg, leafStatus, entries)
    node
  }

  def apply(maxEntries: Int, distThresold: Double, distFunction: DistFunction, mrg: Boolean, leafStatus: Boolean): CFNode = {
    CFNode(maxEntries, distThresold, distFunction, mrg, leafStatus, Vector.empty[CFEntry])
  }

}


case class CFNode(maxEntries: Int,
                  distThreshold: Double,
                  distFunction: DistFunction,
                  merging: Boolean,
                  leafStatus: Boolean,
                  entries: Vector[CFEntry],
                  nextLeaf: Option[CFNode] = None,
                  previousLeaf: Option[CFNode] = None) {


  def setPrevious(newPrev: CFNode): CFNode = {
    this.copy(previousLeaf = Some(newPrev))
  }

  def setNext(newNext: CFNode): CFNode = {
    this.copy(nextLeaf = Some(newNext))
  }

  def isDummy: Boolean = (maxEntries eq 0) && (distThreshold eq 0) && (this.size eq 0) && (previousLeaf.isDefined || nextLeaf.isDefined)

  def size: Int = entries.size

  def findClosestEntry(entry: CFEntry): CFEntry = {
    entries.minBy(f => f.distance(entry, distFunction))
  }

  def mapToClosestSubCluster(e: CFEntry): Int = {
    val closest = findClosestEntry(e)
    closest.child.map(child => child.mapToClosestSubCluster(e))
      .orElse(closest.subclusterID).get
  }

  def insertEntry(cFEntry: CFEntry): Option[CFNode] = {
    if (entries.isEmpty) return Some(this.copy(entries = entries :+ cFEntry))

    val closest = findClosestEntry(cFEntry)
    val idxClosest = entries.indexOf(closest)

    closest.child.map(child => {
      child.insertEntry(cFEntry)
        .map(f =>
          this.copy(entries = entries.updated(idxClosest, closest.update(cFEntry)))
        ).orElse {
        val split = splitEntry(closest)

        split.map {
          splitResult =>
            if (splitResult._1.entries.size > maxEntries)
              Some(this)
            else if (merging) {
              mergingRefinement(splitResult._2)
            }
        }


        null

      }


    })


    if (closest.child.isDefined) {
      val closestIdx = entries.indexOf(closest)
      return Some(this.copy(entries = entries.updated(closestIdx, closest.update(cFEntry))))
    }
null

  }

  def isLeaf(): Boolean = leafStatus

  def resetEntries(): Unit = {
    this.copy(entries = Vector.empty[CFEntry])
  }

  // to be replaced.
  private def replaceClosestPairWithNewEntries(p: CFEntryPair, newE1: CFEntry, newE2: CFEntry): CFNode = {
    this.copy(entries = entries.flatMap { entry =>
      if (entry == p.e1) Some(newE2)
      else if (entry == p.e2) Some(newE2)
      else None
    })
  }

  private def replaceClosestPairWithNewMergedEntry(pair: CFEntryPair, newEntry: CFEntry): CFNode =
    this.copy(entries = entries.flatMap(entry => if (entry == pair.e1) Some(newEntry) else None))


  // this is not so functional.
  private def splitEntry(closest: CFEntry): Option[(CFNode, CFEntryPair)] = {
    // we are sure here there is a child
    val oldNode: CFNode = closest.child.get
    val oldEntries = oldNode.entries
    val p = findFarthestEntryPair(oldEntries)

    val newNode1 = CFNode(maxEntries, distThreshold, distFunction, merging, oldNode.isLeaf())
    val entry1 = CFEntry(child = newNode1)

    val newNode2 = CFNode(maxEntries, distThreshold, distFunction, merging, oldNode.isLeaf())
    val entry2 = CFEntry(child = newNode2)

    if (oldNode.isLeaf()) {
      case true =>
        val (prevNode, nxtNode) = (oldNode.previousLeaf, oldNode.nextLeaf) match {
          case (Some(previous), Some(next)) => (Some(previous.copy(nextLeaf = Some(newNode1))), Some(next.copy(previousLeaf = Some(newNode2))))
          case (None, Some(next)) => (None, Some(next.copy(previousLeaf = Some(newNode2))))
          case (Some(prev), None) => (Some(prev.copy(nextLeaf = Some(newNode1))), None)
          case _ => (None, None)
        }
        (newNode1.copy(previousLeaf = prevNode, nextLeaf = Some(newNode2)),
          newNode2.copy(previousLeaf = Some(newNode1), nextLeaf = nxtNode))

    }
    p.map(redistributeEntries(oldEntries, _, entry1, entry2) match {
      case (redistributedEntry1, redistributedEntry2) =>
        (
          this.copy(entries = entries.filter(_ == closest) :+ redistributedEntry1 :+ redistributedEntry2),
          CFEntryPair(entry1, entry2)
        )
      case _ => (this, CFEntryPair(entry1, entry2))
    })

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
                                  closeEntry: CFEntryPair, newE1: CFEntry, newE2: CFEntry) : (CFEntry, CFEntry) = {

    val allOldEntries = oldEntry1 ++ oldEntry2
    allOldEntries.foldLeft((newE1, newE2)) {
      (previous, entry) => {
        // To be move
        val dist1 = closeEntry.e1.distance(entry, distFunction)
        val dist2 = closeEntry.e2.distance(entry, distFunction)

        if (dist1 <= dist2) {
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
        else {
          previous
        }
      }
    }
  }

  def redistributeEntries(oldEntry1: Vector[CFEntry], oldEntry2: Vector[CFEntry], newE: CFEntry): CFEntry = {
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


  private def mergingRefinement(entryPair: CFEntryPair): Option[CFNode] = {
    val nodeEntries = entries
    findFarthestEntryPair(nodeEntries).map { p =>
      if (p == entryPair)
        return None
      else {
        (p.e1.child, p.e2.child) match {
          case (Some(e1Child), Some(e2Child)) =>
            if(e1Child.isLeaf() != e1Child.isLeaf())
              return None
            if(e1Child.entries.size + e2Child.entries.size > maxEntries) {
              val newEntry1 = CFEntry().setChild(e1Child.copy(entries = Vector.empty[CFEntry]))
              val newEntry2 = CFEntry().setChild(e2Child.copy(entries = Vector.empty[CFEntry]))
              val redistribued = redistributeEntries(e1Child.entries, e2Child.entries, p, newEntry1, newEntry2)
              return Some(replaceClosestPairWithNewEntries(p, redistribued._1, redistribued._2))
            } else {

              var newNode = CFNode(maxEntries,distThreshold, distFunction, merging, leafStatus)
              var newEntry = CFEntry().setChild(newNode)
              newEntry = redistributeEntries(e1Child.entries, e2Child.entries, newEntry)
              // TODO : ITS WRONG
              if(e1Child.isLeaf() && e2Child.isLeaf()) {
                val oldNode = e1Child.copy(previousLeaf = e1Child.previousLeaf.map(prev => prev.setNext(newNode)),
                  nextLeaf = e1Child.nextLeaf.map(next => next.setPrevious(newNode)))
                newNode = newNode.copy(previousLeaf = oldNode.previousLeaf, nextLeaf = oldNode.nextLeaf)
              }
              replaceClosestPairWithNewMergedEntry(p, newEntry)

            }
          case _ => None


        }
      }

        return None
    }
  }
}


class CFTree {

}
