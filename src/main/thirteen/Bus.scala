package thirteen

object Bus extends App {

  def findBus(input: String): BigInt = {
    val (arrivalTime, busesStr) = parse(input)
    val buses = busesStr.filter(_ != "x").map(_.toInt)
    val departure = buses.map(bus => (bus, findSoonestDeparture(arrivalTime, bus))).minBy(_._2)
    departure._1 * (departure._2 - arrivalTime)
  }

  private def parse(input: String): (BigInt, Array[String]) = {
    val split = input.split("\n")
    val arrivalTime = split(0).toInt
    val buses = split(1).split(",")
    (arrivalTime, buses)
  }

  //7,13,x,x,59,x,31,19
  // 59 * x = 31 * y + 2
  // 59 * x     = a
  // 31 * y + 2 = a
  // a = 0 (mod 59)
  // a = 2 (mod 31)
  // 59 * x = 2 (mod 31)
  // 10 * 59 * x = 2 * 10 (mod 31)
  // x = 20 (mod 31)
  // deleni 7 -> zbytek 0
  // deleni 13 -> zbytek 1
  // deleni 59 -> zbytek 4
  // deleni 31 -> zbytek 6
  // deleni 19 -> zbytek 7

  // x = 0 mod 7
  // x = 1 mod 13
  // x = 4 mod 59
  // x = 6 mod 31
  // x = 7 mod 19

  def findPatternChinese(input: String): BigInt = {
    //    val buses = parse(input)._2.zipWithIndex.filter(_._1 != "x").map(pair => (pair._1.toInt, pair._2)).sortBy(_._1)(Ordering[Int].reverse)
    val buses: Array[(BigInt, BigInt)] = parse(input)._2.zipWithIndex.filter(_._1 != "x")
      .map(pair => (pair._1.toInt, pair._2))
      .map(pair => (pair._1, Math.abs(pair._2 - pair._1)))
    //    val buses = buses1.sortBy(_._1)(Ordering[BigInt])
    var result = buses(0)
    for (i <- 1 until buses.length) {
      val next = buses(i)
      val (a, b) = getBezout(result._1, next._1)
      result = (result._1 * next._1, a * result._1 * next._2 + b * next._1 * result._2)
      result = getPositive(result)
      println(result)
    }
    result._2 % result._1
  }

  def getPositive(in: (BigInt, BigInt)): (BigInt, BigInt) = {
    var res = in
    while (res._2 < 0) {
      res = (res._1, res._2 + res._1)
    }
    (res._1, res._2 % res._1)
  }

  def getBezout(a: BigInt, b: BigInt): (BigInt, BigInt) = {
    var (old_r, r) = (a, b)
    var old_s: BigInt = 1
    var s: BigInt = 0
    var old_t: BigInt = 0
    var t: BigInt = 1
    while (r != 0) {
      val quotient = old_r / r
      val newR = getNewPair(quotient, r, old_r)
      val newS = getNewPair(quotient, s, old_s)
      val newT = getNewPair(quotient, t, old_t)
      old_r = newR._1
      r = newR._2
      old_s = newS._1
      s = newS._2
      old_t = newT._1
      t = newT._2
    }
    (old_s, old_t)
  }

  def getNewPair(quotient: BigInt, curr: BigInt, old: BigInt): (BigInt, BigInt) = {
    (curr, old - quotient * curr)
  }


  def findPatternClever(input: String): BigInt = {
    val buses: Array[(BigInt, BigInt)] = parse(input)._2.zipWithIndex.filter(_._1 != "x").map(pair => (pair._1.toInt, pair._2))
    var time: BigInt = 0L
    var diff: BigInt = buses(0)._1
    for (i <- 2 to buses.length) {
      time = getTime(time, buses, i, diff)
      diff = buses.slice(0, i).map(_._1).product
    }
    time
  }

  def getTime(time: BigInt, buses: Array[(BigInt, BigInt)], cnt: Int, diff: BigInt): BigInt = {
    var t = time
    while (!testTime(t, buses, cnt)) {
      t += diff
      println(t)
    }
    t
  }

  private def testTime(time: BigInt, buses: Array[(BigInt, BigInt)], cnt: Int): Boolean = {
    buses.slice(0, cnt).forall(pair => departsAt(pair._1, time + pair._2))
  }

  def findPattern(input: String, start: BigInt): BigInt = {
    val buses = parse(input)._2.zipWithIndex.filter(_._1 != "x").map(pair => (pair._1.toInt, pair._2)).sortBy(_._1)(Ordering[Int].reverse)
    val longestInterval = buses.maxBy(_._1)
    var time = findSoonestDeparture(start, longestInterval._1) - longestInterval._2
    while (!testTime(time, buses)) {
      if (time % ((longestInterval._1 + longestInterval._2) * 1000) == 0) {
        println(s"T: $time")
      }
      time += longestInterval._1
    }
    time
  }

  private def testTime(time: BigInt, buses: Array[(Int, Int)]): Boolean = {
    buses.forall(pair => departsAt(pair._1, time + pair._2))
  }

  private def departsAt(bus: BigInt, t: BigInt): Boolean = {
    t % bus == 0
  }

  def findSoonestDeparture(arrivalTime: BigInt, bus: Int): BigInt = {
    val atLeast = arrivalTime / bus
    var dep = atLeast * bus
    while (dep < arrivalTime) {
      dep = dep + bus
    }
    dep
  }

  val input = "1007153\n29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,433,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,977,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,41"
  //  println(findBus(input))
  //  113320627505000
  // 126215987977000
  //  println(findPattern(input, 100000000000000L))
  //  println(findPatternChinese(input))
  println(findPatternClever(input))
}
