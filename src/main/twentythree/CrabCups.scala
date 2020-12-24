package twentythree

object CrabCups extends App {

  def getLabels(input: String, n: Int): String = {
    var labels = input.split("").reverse.map(_.toInt).toList
    var cups = Map[Int, Cup]()
    var last: Cup = null
    for (i <- labels.indices) {
      val newCup = new Cup(labels(i), last)
      cups += (labels(i) -> newCup)
      last = newCup
    }
    cups(labels.head).next = last
    cups += (labels.head -> last)
    var cur = cups(labels.last)
    for (i <- 0 until n) {
      val curNext = cur.next
      cur.next = cur.next.next.next.next
      val dest = findDest(cur)
      val destNext = dest.next
      dest.next = curNext
      dest.next.next.next.next = destNext
      cur = cur.next
    }
    val sb = new StringBuilder()
    var c = cur
    while (c.label != 1) {
      c = c.next
    }
    c = c.next
    for (i <- 0 until 8) {
      sb.append(c.label)
      c = c.next
    }
    sb.toString()
  }

  def getCorrectLabels(input: String, n: Int, max: Int): BigInt = {
    var labels = input.split("").map(_.toInt).toList
    var cups = Map[Int, Cup]()
    var last: Cup = null
    for (i <- max - 1 to 0 by - 1) {
      val label = if (i < 9) labels(i) else i + 1
      val newCup = new Cup(label, last)
      cups += (label -> newCup)
      last = newCup
    }
    val maxLabel = if (max < 10) labels.last else max
    cups(maxLabel).next = last
    var cur = last
    for (i <- 0 until n) {
      val curNext = cur.next
      val dest = findDestImpr(cur, cups, max)
      cur.next = cur.next.next.next.next
      val destNext = dest.next
      dest.next = curNext
      dest.next.next.next.next = destNext
      cur = cur.next
    }
    var c = cups(1)
    val a = c.next
    val b = c.next.next
    a.label.toLong * b.label.toLong
  }

  private def findDestImpr(cup: Cup, map: Map[Int, Cup], max: Int): Cup = {
    val picked = Set(cup.label, cup.next.label, cup.next.next.label, cup.next.next.next.label)
    var destLabel = cup.label - 1
    while (picked.contains(destLabel) || !Range(1, max + 1).contains(destLabel)) {
      destLabel -= 1
      if (destLabel < 1) {
        destLabel = max
      }
    }
    map(destLabel)
  }

  private def findDest(cup: Cup): Cup = {
    val originalLabel = cup.label
    var destLabel = cup.label - 1
    var next = cup.next
    while (next.label != destLabel) {
      next = next.next
      if (originalLabel == next.label) {
        destLabel = destLabel - 1
        if (destLabel < 1) {
          destLabel = 9
        }
      }
    }
    next
  }

  println(getLabels("284573961", 100))
  println(getCorrectLabels("284573961", 10000000, 1000000))
}

class Cup(val label: Int, var next: Cup) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Cup]

  override def equals(other: Any): Boolean = other match {
    case that: Cup =>
      (that canEqual this) &&
        label == that.label
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(label)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
