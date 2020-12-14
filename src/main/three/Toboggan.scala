package three

class Slope(val map: Array[Array[Char]]) {
  def get(coord: Coord): Char = {
    map(coord.y)(coord.x % map(coord.y).length)
  }

  def depth(): Int = map.length
}

case class Coord(x: Int, y: Int) {
  def get(direction: String): Coord = {
    get(direction, 1)
  }

  def get(direction: String, steps: Int): Coord = {
    direction match {
      case "N" => Coord(x, y - steps)
      case "S" => Coord(x, y + steps)
      case "E" => Coord(x + steps, y)
      case "W" => Coord(x - steps, y)
      case "NE" => Coord(x + steps, y - steps)
      case "NW" => Coord(x - steps, y - steps)
      case "SE" => Coord(x + steps, y + steps)
      case "SW" => Coord(x - steps, y + steps)
    }
  }
}

object Toboggan extends App {
  def countTrees(input: String, right: Int, down: Int): Int = {
    val slope = parseSlope(input)
    var coord = Coord(0, 0)
    var treesCnt = 0
    while (coord.y < slope.depth()) {
      if (slope.get(coord) == '#') {
        treesCnt += 1
      }
      coord = Coord(coord.x + right, coord.y + down)
    }
    treesCnt
  }

  def countMultipleSlopes(input: String): Int = {
    countTrees(input, 1, 1)*countTrees(input, 3, 1)*countTrees(input, 5, 1)*countTrees(input, 7, 1)*countTrees(input, 1, 2)
  }

  private def parseSlope(input: String): Slope = {
    new Slope(input.split("\n").map(_.toCharArray))
  }

  val input = "........#.............#........\n...#....#...#....#.............\n.#..#...#............#.....#..#\n..#......#..##............###..\n..........#......#..#..#.......\n.#..#.......#.........#.#......\n.........#..#....##..#.##....#.\n..#....##...#..................\n##..........#.##...#....##..#..\n...#....#...#..............#...\n...........................#..#\n..##.##.#..................#...\n...#.##..#............#........\n........#.......#...#.....##.#.\n.##..........#......#.......#..\n...#..........#...#..#.......#.\n......#...#...#.##.......#.#...\n........#...#...#...##.........\n#..............#.#....#.......#\n..#..#..#.#....#...............\n.....#........#...#..........#.\n##......#...#..#.##.......#....\n..#.#.....#.#.............#.#.#\n#..#..##......##...#...........\n..#......#........#.....#......\n.....#.......#....#.#...#......\n...#........#...........#...#..\n.......#.#...........###....#..\n...#...........##....##........\n#....#..####....#.....#..#....#\n..........#...........#........\n...#.......#....#.#.........#..\n....#...#.......#..###.........\n......#......#..#......#..#....\n...#.....#............#..#.....\n...#.#.#.#..#.......#.....#....\n#....##...#.........#...##.....\n#..#.......#..#..#..#...##.....\n#.......#............#.....#...\n.#........##....##...#........#\n.....#...#.....................\n.......#........#..............\n.....#............#.#.#...#.#..\n.....##..#.............#.......\n..#.##..#........#..#...#......\n.........#.#....#...........#..\n.#.....#..#....#.....#...#.....\n....#.#................#.......\n...............##......#...#...\n.##...#...#.......##.#....#....\n............#........#.......#.\n......##.#.#...................\n.#.#..............#.......#....\n#.....#...#.......#..#...#.....\n.............#....#..#......#..\n........#...##................#\n.......#...#..#..##............\n..#..#...##...#..#.#.....#...#.\n.#.#...#.........#.#...........\n...###....#.......#...#........\n........#......##.#...#..##..#.\n.....................#.#.......\n.............#...........#...#.\n#..#..#.....#.#...#............\n...#....#.....#...........#....\n..##.....##...#......#..##.....\n#.....#.....###.#.....#....##..\n.#...........###...............\n..................#..##.#...#..\n................#....##.#......\n.#.#.#...#....#.........#..#.#.\n#.......#........##............\n.......##.#....#.#............#\n..........#..##.#....#.........\n........##..#....#.............\n.........#....#...........##...\n#.........#.#..#..#..........#.\n.....#........#......#.........\n....#.#.#...............#......\n.#..#..##...#.##..........#....\n..#....................#.#.....\n.........#....#...........#.#.#\n........#....##.##.............\n..#.....#.......#..#......#....\n#..........#.#.....#.#....#....\n........##.#.....#..#.....#.#..\n...................#...#....#.#\n............#..#....#...#...#..\n..............#.#.........#....\n...#..#..#.#..##..##...........\n.#...........................#.\n.#.......#...........#....#.#.#\n......#..#...#........#...##...\n.........#......#.#.......#...#\n...#..##................#......\n.............#.#..##....#.#....\n...............#..#......#.....\n.#......#.#.#....#........#....\n........#..#.##..#..#.........#\n...#....#.#...#..#.......#..#..\n..#...##.........#..#...#......\n...#...........#.............#.\n....#.....................#....\n.....#..#...............#.#...#\n....#..........#........#......\n..#....#........##..##.........\n...#....#..#.#.......#...#.....\n..#........#....#...##....#.#..\n.#...#........##.....#....###..\n#....#....##......#........#...\n.........#..#.#..........#....#\n....#...#.....#.......##.......\n..............#..........#.##..\n#...#..#..............#......#.\n.................#......##....#\n..#..##..#.......#..#.#......#.\n.............#........#.....#.#\n.#.##............#..#..........\n..#...#...........#..##........\n.#....#...#....#.......#.......\n...#.#..#..#..#....#.....#..#..\n....#..##..............#...#...\n#..........###......###........\n.##.##......#..#............#..\n.#...........#.#.....#...#.....\n#.#..#...#............#........\n.........#...#...#..........##.\n.......###..#..........#.......\n...........###.....#........#..\n.#.............#.....#......#..\n...#.....#....#.#.........##...\n....##..##...#.......##........\n......#....##.........#......#.\n..........#.....##..#.....#..#.\n..........####...#..#.........#\n.##....#..#.#...#.......#......\n...#.#.##.#.#...#....#.#.#.....\n.........#...##........##.....#\n..#........#..........##...##.#\n##...##..........#.#...........\n..............#......#.........\n........#.....#.#.......#......\n.#...#.....#....#.#..#.........\n.....#....................##...\n....#..................#.#...##\n.....#............#..##........\n#..........#....#.#.......##.#.\n....#..#.....................#.\n#..#....##.....#...............\n..#...#..#..##....#.#..........\n.......#......#.#.......#.....#\n...#.#.......#...#.##..........\n....#..........#....#.#.#......\n.......#..#..........#..##.....\n#......#......#...#......#...#.\n###..#....##......##........#..\n.#..........#.....#.......#.#..\n.......#.....#.....#.#.........\n..#...#....#...................\n..............#.##.............\n.#...#.......#.##...#.#.......#\n.......#......................#\n....#.#...#.#........#.........\n.#......#....#...#.............\n#.......#...###.....#.#.#..#...\n#....##.#...............##.....\n..#.......#..................#.\n.....####...............#......\n.##......#......#.#.......##.#.\n#......##..###....#....#......#\n.##.......##.##...#.##.........\n......##............#.......#..\n......#..#.....##.#............\n.#..........#.....##...........\n#.........#......#......##.#...\n.........#.......#..#......#.#.\n.........#.......#...........#.\n.#..##.#..................##...\n.............#.............#...\n.....##........#......##...##..\n..#..#.#.....#..#....#.........\n.....#....#.....#.....#........\n#......##.....#....#....#......\n#.................#..#.#......#\n.......#..#......#....#.#...#.#\n....#.........#..#..........#.#\n##......#............#...#...#.\n....##......#...#.....#....##..\n.#...##.........#..............\n......#.....................#..\n..#..........###....#..........\n#....#...#..#.............#....\n#........#.#......#....#.......\n.#...#.......#..#...#.#...#..#.\n................##.#.....#.....\n###.......#...#................\n...#.......#...#.#.....#.......\n..#.........#.....#.#.......#..\n......#.......................#\n#.....#.#..#....#.......#......\n...#....#..#....####...........\n.............#.....#...##......\n.......#.........#...#..#......\n.##..#.........#....#.#........\n....##...#.#...........#....#..\n.........................##....\n..###.......##....#.#.........#\n.#....#.#.#...........##....#..\n......#...#..#..#..#..#.......#\n..#....#.#.......#..#..#..#...#\n.....##...#.##....#.#...#......\n.........#..#....#..#..........\n.##..##.........#.#.....#......\n..........#...##...#.#...#.....\n#.##..#..#.............#.......\n...#...........#.......#......#\n.......#....#....#...##.......#\n..#.##........###..#......#....\n...#...........###......#..#..#\n.#.........#.#.........#.#.....\n##.......##.##.##......##......\n............#...#..........#...\n....................#..........\n...#..#...........#...#...#....\n.................#...#......###\n...#................#.#.##.....\n...............#........#......\n#.............##......#.#..#...\n..#.#.....#..#.##.....##...#...\n......#.........#......#.......\n#.......#......#....#........#.\n.#..##.....#.........#.........\n....##.##.#...#.........##.#...\n...............#..#..#..##.....\n.#..#...............###........\n.##............##..............\n...............#...##...#...#.#\n..#.#......#.#..#.............#\n#.#..#..##.........#.#.#...#...\n....##.#....................##.\n.........#..#.....#.....#..#..#\n....#......#......#.##....#....\n........###..#.............#..#\n##................#.........#..\n#.....#.......#....#...........\n..#.......#..#........#....#...\n..#.#.##..#.#...##........#.##.\n..#..........#............#....\n..........#...............##...\n..........###........#.#.......\n.....###..#.............#......\n##.............#...#.....#.....\n.....#......#....#........#.#..\n............#..#..............#\n.................#...........##\n#........#.........###.....#...\n..#.#..............##......#.#.\n.#...........#.........#..##..#\n...............................\n.#.....#..#....#....#......#...\n.#...#......#.#..#....#.......#\n......#.##.......#......#......\n......#..###..#................\n#..#.....#........##...#.......\n......##.........##....#...##..\n.#..........#.................#\n#..#.......#...............#...\n.........#..###....#.#.##.#....\n..#...#.##..##...............##\n.........#.....................\n.#....##...#......#....#.......\n............#..........#..#....\n...#......##....#....#........#\n.#...................#.........\n#.#........###....#..........#.\n.........#....#....#........##.\n.#....#..#.........#..#........\n...............#..#...#..#...##\n.........#....##....#......#...\n.#.............................\n...#........#...#.#...#.#..#...\n.....#..##...#.#...............\n#.....#....#.........#.........\n#...#...........##.........#...\n..##........#.#...#...#......#.\n...........#.....#...#.#.......\n......###....#.....#...........\n......##...#..........#....#.#.\n.......##..##..........#.......\n....#............#..#....##....\n..##...................#.#.....\n...#.#..#.#....................\n.#..##..#............##.###..#.\n#.#...#....#.#..........#.#....\n........#....#.....#...........\n..##....#...#.......#..........\n...........##.##....#..........\n.....#............#............\n.......#.............#....#....\n.................#......#......\n......##.......#....#..##...#..\n.#..#....#.....................\n...#.#.#...#......##...........\n##........##.#....#....#.......\n.......#.....#..#..#...#.##....\n#..........#....#.#..#..#..#...\n...##..............#...........\n.........#.....#.#....#.......#\n.........#....##..#..##..#.....\n.....#......................#..\n...###...#..#......#...........\n....#.....................#....\n...............................\n..#.....###.......#..#....#....\n#..........#.................#.\n......#.......###.......#..##..\n.............#.##..............\n......#..#.#..#...........#....\n...#....##.#...#..#.#...#....#.\n..................#...#....#.##\n......#.#....#.................\n......#.#.....#.....#..##......\n#..##...........#..#.....#.##.."
  println(countTrees(input, 3, 1))
  println(countMultipleSlopes(input))
}
