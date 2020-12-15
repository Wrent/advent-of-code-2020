package fifteen

object Elves extends App {
  def getNth(input: String, n: Long): Long = {
    val numbers = input.split(",").map(_.toLong)
    var map = Map[Long, List[Long]]()
    numbers.zipWithIndex.foreach(entry => map = map + (entry._1 -> List(entry._2)))
    var last = numbers(numbers.length - 1)
    for (i <- numbers.length until n.toInt) {
      if (map(last).length > 1) {
        val next = add(i - map(last)(1) - 1, i, map)
        last = next._1
        map = next._2
      } else {
        val next = add(0L, i, map)
        last = next._1
        map = next._2
      }
      if (i % 10000 == 0) println(s"$i $last")
    }
    last
  }

  private def add(next: Long, position: Long, map: Map[Long, List[Long]]): (Long, Map[Long, List[Long]]) = {
    val resultMap: Map[Long, List[Long]] = if (map.contains(next)) {
      val list = map(next)
      map + (next -> (List(position, list.head)))
    } else {
      map + (next -> List(position))
    }
    (next, resultMap)
  }
  //168269 0
  //168270 6
  //168271 23
  //168272 714
  //168273 13533
  //168274 4381
  //168275 152016
  //168276 0
  //168277 7
  //168278 21
  //168279 180
  //168280 1427
  //168281 8208
  //168282 0
  //168283 6
  //168284 13
  //168285 29
  //168286 143
  //168287 1879
  //168288 9581
  //168289 86379

  val input = "0,5,4,1,10,14,7"
  println(getNth(input, 2020))
  println(getNth(input, 30000000))
}
