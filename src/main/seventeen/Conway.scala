package seventeen

object Conway extends App {
  def getActiveCubes(input: String, cycles: Long): Long = {
    val split = input.split("\n").zipWithIndex.map(_.swap)
      .map(pair => (pair._1, pair._2.split("").zipWithIndex.map(_.swap)))
    var map: Map[Coord3D, String] = Map()
    split.foreach(row => {
      row._2.foreach(cube => {
        map = map + (Coord3D(cube._1, row._1, 0) -> cube._2)
      })
    })
    for (i <- 0L until cycles) {
      map = iterate(map)
    }
    map.count(entry => entry._2 == "#")
  }

  def getActiveCubes4d(input: String, cycles: Long): Long = {
    val split = input.split("\n").zipWithIndex.map(_.swap)
      .map(pair => (pair._1, pair._2.split("").zipWithIndex.map(_.swap)))
    var map: Map[Coord4D, String] = Map()
    split.foreach(row => {
      row._2.foreach(cube => {
        map = map + (Coord4D(cube._1, row._1, 0, 0) -> cube._2)
      })
    })
    for (i <- 0L until cycles) {
      map = iterate4d(map)
    }
    map.count(entry => entry._2 == "#")
  }


  def iterate(map: Map[Coord3D, String]): Map[Coord3D, String] = {
    val additional = map.flatMap(entry => entry._1.neighbours()).toSet
    var result = map
    additional.foreach(coord => {
      if (!result.contains(coord)) {
        result = result + (coord -> ".")
      }
    })
    result.view.map(entry => (entry._1, getNext(entry._1, result))).toMap
  }

  def iterate4d(map: Map[Coord4D, String]): Map[Coord4D, String] = {
    val additional = map.flatMap(entry => entry._1.neighbours()).toSet
    var result = map
    additional.foreach(coord => {
      if (!result.contains(coord)) {
        result = result + (coord -> ".")
      }
    })
    result.view.map(entry => (entry._1, getNext4d(entry._1, result))).toMap
  }

  private def getNext(coord: Coord3D, map: Map[Coord3D, String]): String = {
    map(coord) match {
      case "#" => if ((2 to 3).contains(countActiveNeighbours(coord, map))) "#" else "."
      case "." => if (countActiveNeighbours(coord, map) == 3) "#" else "."
    }
  }

  private def getNext4d(coord: Coord4D, map: Map[Coord4D, String]): String = {
    map(coord) match {
      case "#" => if ((2 to 3).contains(countActiveNeighbours4d(coord, map))) "#" else "."
      case "." => if (countActiveNeighbours4d(coord, map) == 3) "#" else "."
    }
  }

  private def countActiveNeighbours(coord: Coord3D, map: Map[Coord3D, String]): Int = {
    coord.neighbours().toList.map(map.get)
      .filter(_.isDefined)
      .count(_.get == "#")
  }

  private def countActiveNeighbours4d(coord: Coord4D, map: Map[Coord4D, String]): Int = {
    coord.neighbours().toList.map(map.get)
      .filter(_.isDefined)
      .count(_.get == "#")
  }

  val input = "##....#.\n#.#..#..\n...#....\n...#.#..\n###....#\n#.#....#\n.#....##\n.#.###.#"
  println(getActiveCubes(input, 6))
  println(getActiveCubes4d(input, 6))
}

case class Coord4D(x: Long, y: Long, z: Long, w: Long) {
  def neighbours(): Set[Coord4D] = {
    var res = Set[Coord4D]()
    for (i <- -1 to 1) {
      for (j <- -1 to 1) {
        for (k <- -1 to 1) {
          for (l <- -1 to 1) {
            res = res + Coord4D(x + i, y + j, z + k, w + l)
          }
        }
      }
    }
    res = res - this
    res
  }
}

case class Coord3D(x: Long, y: Long, z: Long) {
  def neighbours(): Set[Coord3D] = {
    Set(
      Coord3D(x, y, z + 1),
      Coord3D(x, y + 1, z + 1),
      Coord3D(x, y + 1, z + 1),
      Coord3D(x - 1, y + 1, z + 1),
      Coord3D(x + 1, y + 1, z + 1),
      Coord3D(x, y, z + 1),
      Coord3D(x + 1, y, z + 1),
      Coord3D(x - 1, y, z + 1),
      Coord3D(x, y - 1, z + 1),
      Coord3D(x - 1, y - 1, z + 1),
      Coord3D(x + 1, y - 1, z + 1),

      Coord3D(x, y + 1, z),
      Coord3D(x, y + 1, z),
      Coord3D(x - 1, y + 1, z),
      Coord3D(x + 1, y + 1, z),
      Coord3D(x + 1, y, z),
      Coord3D(x - 1, y, z),
      Coord3D(x, y - 1, z),
      Coord3D(x - 1, y - 1, z),
      Coord3D(x + 1, y - 1, z),

      Coord3D(x, y, z - 1),
      Coord3D(x, y + 1, z - 1),
      Coord3D(x, y + 1, z - 1),
      Coord3D(x - 1, y + 1, z - 1),
      Coord3D(x + 1, y + 1, z - 1),
      Coord3D(x, y, z - 1),
      Coord3D(x + 1, y, z - 1),
      Coord3D(x - 1, y, z - 1),
      Coord3D(x, y - 1, z - 1),
      Coord3D(x - 1, y - 1, z - 1),
      Coord3D(x + 1, y - 1, z - 1),
    )
  }
}
