package twentyfour

import java.util.regex.Pattern

object Tiles extends App {

  def countBlack(input: String): Int = {
    getInitial(input).values.count(it => it.color == "B")
  }

  def countBlackAfter(input: String, n: Int): Int = {
    var map = getInitial(input)
    for (i <- 0 until n) {
      map = iterate(map)
    }
    map.values.count(it => it.color == "B")
  }

  def iterate(map: Map[(Int, Int), Tile]): Map[(Int, Int), Tile] = {
    val additional = map.values.flatMap(_.neighbours()).toList.distinct
    var result = map
    additional.foreach(coord => {
      if (!result.contains(coord)) {
        result = result + (coord -> new Tile(coord._1, coord._2, "W"))
      }
    })
    result.view.map(entry => (entry._1, getNextDirect(entry._1, result))).toMap
  }

  private def getNextDirect(coord: (Int, Int), map: Map[(Int, Int), Tile]): Tile = {
    val color = map(coord).color match {
      case "B" => if (countBlackNeighbours(coord, map) == 0 || countBlackNeighbours(coord, map) > 2) "W" else "B"
      case "W" => if (countBlackNeighbours(coord, map) == 2) "B" else "W"
    }
    new Tile(coord._1, coord._2, color)
  }

  private def countBlackNeighbours(coord: (Int, Int), map: Map[(Int, Int), Tile]): Int = {
    val tile = new Tile(coord._1, coord._2)
    tile.neighbours()
      .map(map.get)
      .filter(_.isDefined)
      .map(_.get)
      .map(_.color)
      .count(_ == "B")
  }


  private def getInitial(input: String): Map[(Int, Int), Tile] = {
    val identifiers = input.split("\n").map(parseDir).toList
    var map = Map[(Int, Int), Tile]((0, 0) -> new Tile(0, 0))
    identifiers.foreach(instruction => {
      var curr = (0, 0)
      instruction.foreach(dir => {
        curr = new Tile(curr._1, curr._2).get(dir)
      })
      val color = if (map.contains(curr)) {
        if (map(curr).color == "W") {
          "B"
        } else {
          "W"
        }
      } else {
        "B"
      }
      map = map + (curr -> new Tile(curr._1, curr._2, color))
    })
    map
  }

  private def parseDir(line: String): List[String] = {
    val pattern = Pattern.compile("(e|se|ne|w|nw|sw)")
    val matcher = pattern.matcher(line)
    var matches = List[String]()
    while (matcher.find()) {
      matches = matches :+ matcher.group()
    }
    matches
  }

  val input = "swsenenwwneeseswswsenwnwenewenwnwse\nwnewnweswswsenewnenwnwseseweeseswne\nswwnewwswswswswswswsweeswwwswnwew\nseeneeenwweenwnweswwseeeee\nswswseseseswsesesesenwswseswsese\nseseseneswwnesenwseseswwnenwseenwsesw\nnwseseswswenwnwswswneseswswseswseswswswnwse\nenwsenwnewweeeswneenenwesweneswene\nnwnwnwneswsenenwnwwsenwswnewnee\nsesewsesenwseeseneseseseswswseswswsese\nswsenwnwswnwnwneswwnwnenwwwnwnwnenwnw\neseswwwwswwwnewwwwwwswnewsww\nneswenwenwswseenwseeneenweenenee\nesenwnwwsenwsenenwsenwwnwnwewnwnwwnw\nnwswesweseseeeeeeeesenese\nneneeneesewsewswenenwswnw\nseeneneewneneneneweeneswsenenenee\nenwwneeeswneeenweseeeeseneenee\nswwswsesesesewneseseseeseseseseseene\nnwsewwweswnesewwneseeswwwenwwnw\nnwnenenenwnwswswwnwwnewneenwnwsenenenwse\nnwsenwnwnenwsewnenwwwnwnwnwwnwwnww\nnewsenewwwwwwswnwwsesenwwwwwse\nsenweeneeneneeneneeswnenenenewenewe\nwwnwneswsewnwnenwesenenwneneneneneenene\nsenwnwwwnwnwnwnwnwenenwnwnwswnwnwnwsewnw\neenwnenenwseneneneswwneeeseseeseenwne\nnwswewewneenwsweneeweneneneee\nswwnwseswswswnweewnwwesw\nwnwwwwwwwwewwwnw\nswnenwswnenenwnwnwsewnenwnenenwnwsenenw\newwswswswneswnwwswwwsewwwnwwswsw\nwwseswneswwwwneswwesenwwwww\nneswswwenwswswswnwswseswswswswswswewswsw\nnwswnwswswswswswswseeswswwswswswswswesw\nsenwwswwnwnenesesewwnenwswwnewwwse\nneneneneeneswsweeneneneneswneeenwsenw\nswnwneswswwseswswswswswswsweswwsww\nwsenewewenwwwwnwsenwnweww\nswnwswswsenwswswnwswswswswsesweswsweesw\nnenenwnenenenenenwneneswsewnwnwnwsenwnee\neseeswnwseneseenweeesenwweenwnwsw\neeeswnenenewenenenewneneeeneeese\nswswswnwseeseswswseseseenwswswse\nswseneseesewnwwnwsenwnewnewneseww\nswwwnewwwsenwwnwswwewnenwnesww\nswseewseeeseeesenwsenesenweeeswe\nnwnwnwnwweswnwnwnwnwwswnwnwnwnwe\nnwwnenwnwwnwnwswsenwnwnwwsenesenwnwne\nseeseseeswseneeeseeseneenweesw\nwneswnwswnenenenenwnenenenenesenenwnwnw\neseseseswewseseweeneeeeswneese\nnesenenenenenwswnenenwnenwneswnenenenene\nnwwnwsenwnwnwwnwwnwnwnwesewesenenwnwnw\nnwnewnwseweneswnwwnw\nsenenesenwneewswewnenenwnenenenwnwne\neswwseswwswnwswswwwswswnewswswswsw\nnenenenwneenenesenwsweseneesweeneene\nwseneneenewnenenesewnweweneneesw\neeeeesenweweeeeneeswnewewene\nnwneneeswwneseeswneneneneenenenenwnwsee\nneswseneneenewnewwesweswnee\nsenwweneswwwsenenewnenwseewwsesww\nnwseneeenwnewsweeeneeeeenenee\nnwsewnwesweswenwseswneseesese\nswseneswnwneneseseswswswswswswswswswswsewse\nnwseneseeseswswswseswesesewswsewwne\nenwseewsesenwsewnewwnenenenenenwsw\nswnwwswsenwewswswwsw\nwwewwwwnwwwnwwewwwwsw\nnwnewnwnwwnwnwneswnwnwneswsenww\nwswnewwwswswswwwnewweswwswwse\nswnwweesenewwnesenesenenwwseneene\nnwnenwwesenwsewnenwnwwnwnwnwsenw\nnewnwwnwsenwnwnwsewwnewwsewwsew\nswneseswswswsweswswswwswswsenenewswswsw\nswsweseswswseseswnweseswwnesesesesesenwsw\nneneneneneswneeneeewnenewneene\nsewsenwneswewwnewswwswswwwwswwww\neweeeseeesesesesenwese\nwnwseswnewseneenwweneeswweeesene\nsesewseseseesesesenwsweesese\newnwnwsesenwnesewewswnwnwneswnwwww\nnwwwnenesewnwnwwwnwwwsww\nneswwwnenwseewsesweseeene\nseseswesenwswseswsese\nsenwweseseseseseswnwseswswswsweeswse\nswwswswwwwsenenewwweswwnwswswesw\neeeenwneenwnewswnenenewswnenenee\nswwnewswswswswneswswswwsw\nseeswnewswwwseeenwwswswnew\nwnwswswswswswneseswswswswwswnesweswswswsw\nnenwnenwnwnwnwneswnwnwneseneeneswnwwne\nswneneeeeeesweeeswenweeneenee\nseseneswnwsweswseswseswsenwse\nswewewnweeswwswwewsewnwnwnew\neeeeweeneeeeeneeneseneswesw\nwwswwnewwwswwsewnwe\nwswswwnwswsweswseswsewwsenwnwwnenene\nneeeeswnewseesesweeseseeneesee\nnwnwnwnwnwsenenwwnwnwnwwnwnwnenwnwesenw\nswseseswneswseswnwseswswswsese\nwwswwwwnwnwnenwnesenwswwnwswnwnenw\nwwswswseswwswwswswwwne\nnwnwnwnwwnwseneesewnwnwswweneewesw\nnesesweeeneeeseseneseseswwseeesese\neesweeseenwweeweneeesene\nseseneseseseswseswsenwewsesewwewese\nwsweseseenwseseesewneeeesesesenwese\nwnwnwenweneswnwnenwnwswnwnwnenwwnwsenwnw\nnwsenenenenwnwnwwnenwnene\neeenwnesenwseswsesenwesesweseseswe\nnwnenwneswnwnwnenwnenenenwnwneswnweswnw\nweweeeeeeseesenwneneweeeee\neewsweeenweeeseenweseeswneene\nswswwswswnwewnewswseeswswswswwwnesw\nwsewwswnewwneewwwwsewwenewww\nnwswsesweneenweewweeseeewsewne\nnwseswseswswswswswswnweswswsweswswswswnwsw\nsenwnwwnenwnwwsewwwnenwsenwsenwnwww\nswswenwswswsewneswswswwswwnwswenwwsw\nnwewwwwsewsenwnwswwwnenesewnwnw\nneeesweseeeeneeesewsewwsenenesesw\nsewswswswneswneneswneesewswswwnewsenw\nwsenwneneneneswnewswseswsweswwwswesw\nseswseseswnwenwswseseseseenenwseseesesesw\nenwneweeeeeswe\nseeswesenewneeseenwsewnwsenesenwsew\nwswwnwswwweneswwswswswswnwwenesw\nnewnwsenenenwenwwwnweneeswnwnwnenenw\nnenwnwseswseneswseswseeseeseenwwswese\nnewenewswwwwwnwwsesw\nwsewwwnwwsesewwwwnewnenwsewww\nseseseswseseswnenwneseseseseswsewseseswnw\nswswsweswnweswwseswswswswenenwwswsww\nnwnwnwnwwweswnwwwwnwswenwwnewewnw\nnenenwnwsenwnenwnwnwnwwnenwnwnw\nseseseswsesenwsenesewseswseswsesesesenw\nneeswnwnwswnweeeneneeswneesw\nnenwsenenenwswnwnenwnenenwnenwnw\nnwwseseseseswsweenesenwseswseeeseese\neneeenewneeeneneneee\nneeneneswnewnwnwnwnwnwnwnwnwnwnewnenwse\nwewseeseseenweesenwseseeswsesese\nwwnwnwwsesewseeewswewnwnwwne\nwseswwswnwnwsesenweenwnweeswnewswsw\neewweeswnesesewweneswnesesenwsw\nnwnwnwewnwnwwwwnwswnwwnenwse\nswnwswneswnwseeswseneswsesesesenw\nsweeeseesenweneeeswseseenwwsenenw\nnenesenesweswneesewswneeswwwswsww\neenesewnwwswsenewwnwnenwsenenwenw\nswnenwswnenenewnenesenenenenenenwnwenenw\nseeseseeswneeewseewnwseseseeseee\nseewwweweeneeseneeneeeenenew\nnwnwnwneswnwenenwswnwwnwnwnwnesweenwnw\nswsesweswwswwwneneswswwswswseswnwsw\nseswswswswsweneneswwseseseseswswseswwse\nnweeseeeseswneeewesesenenw\nswwneseseswswswseneswneswseswseseseswswse\nseneweswnwweseseseeeneswsesesenwse\nwsesenwswsesweweesesenwswnesewsw\neeseeeeeewswenweneesweeee\nswnwnwswnwnwenwnwnenenwneenwswnwnwnwnwnwnw\nsesesenesesesesesesesesesesenwsenesewwsese\nnwnwnwnwnwnwnwnenwnenwnwnesenww\nnwnwnenwewwswwseewewwswswswwwsw\nsenwwseeseswneswswseswsw\nnwnweneseseseseseswswswseswseswsesesee\nseeswenweeeeeseeseeee\nseswseseswseeenwwnenwesesweseswnwsw\nseseseseseeseweeeeseesewese\nswneneenenenwnenenwnenenenw\nwsewnwnwnwwnwwenw\nesenweeeeeswenwsee\nsenenwwenwnwwnwswwwnwnweenwnwnwwsw\neeeeeeenweseeeese\nswnwwsewenwsenwewwnwnwnwenwnwwww\nneneeewswenenesenenenwnwnenewnwnene\nenwwnwwwwsenwneswnwwwnwnwsewnwwwe\nsewswswwswseseseseseseseesesenesenwsw\neseseswseesewnesesesesenwesesesese\nseeesenwnwnweewswnewseeneeneswne\nwwwwwwswseewwwnwsewneswwww\nnwswwewnenwswnwenenenwwnweeenenwne\nwwneeeseswseswswnwsesenwneswswnesesw\neeeneneneeneeneneeww\nnenwwwnwsenwnweswwseewnesenwnwnwne\nnenewneswwwewnewseewwsewsesw\nneneswneswwsewneesewwnenwneenenese\neswenesewwnenwwsewwswneswnwswwne\neswenwseeenesesewsweenweseeesenee\nnwnwnenewnenenwnenwsenenwneneneswenwnw\nneenenwswneneswnwnwewnwewnenwneesew\nnenenwnweneneswnwnesenwnenwnwnwse\nwneseseeswseswswswswesenwnwsesesesenwnw\nneneeeeneneneneswneenenenenenw\nwwswnwwneeewwesewwwwwsenwnww\nnwswswnwseseswswnewswswswswswwswswswe\nwnwewnwnwneneeseneeesewswneneewse\nwwsewseewwwnewsenwwwwwesenwnw\nswwnesenwsenwwnwesenwnenwwnwsewnenwne\neeesewswenwsweeewesewnwneese\nnewseswswwswwwsewswsewwswswnewnwsw\nwnwnwnwwnwenwenweswnwnwswnwnwnwnwnwsw\nnwenwnenwsesewwwneewwwwwseswnwse\nsesweeneenwnwswenwwseseneeenwswese\nweseseesesewsweeenwneneeseeeene\nswsewswsweseswswswseeewnwseseswsesw\neneswnesenewnwnenenwnwnweswwswnwswnee\nnwswewwseswwsweewseseneesenwnesw\neswseseseenwnwsweeesewewnesenwse\nswswwnwswnwweswnewwwswseswwwswww\nseenwwneeeeswenwswseeeeenwene\nswnewwnewwwswswwwswweww\nswseneseswseswneweseswswesenwne\nwsewwwwwwnwwwnwnwwwsewnewwe\neeeseneswneenweswnwneeneeenwnee\nnwnenwswsenenenwnwnwnwswnwnwnwnwnwnwesw\nsenwnwwsenenenwnewnwnenwsenwnenenwnwnee\nneswswswswwneeswswnewsenenwesewswsee\nnwseeeeeseeswwenweeneseseeeee\nseseseseswswseswsenwnwewwseseseenesee\nesweeeenweese\neneneneweneneeeseeeneenwe\nsenwnwsesesesewswswswseesesesenenwsesw\nsenewesweswswnwnwswswswswswswsw\nneseseesewswwsenweswnesesesenwenwsww\nswnwswswswswswsesesesesenwseswswsw\neseweswneeswwsesesewsenwsesesesenese\nneneenweeeeeesweeeeeenwesesw\nneeeneeswenwneeswenwe\nwnesweneseswwseswswwnesewswnwnewne\neseweseeneneeeeneeseneswswswsese\newnenwnwneneenenwnwswnwnenwnwnwnesenwnw\nenewswneneneneneneenewne\nseswnwswwswswsenenewswnwseneswenenenew\nenwewwenenweseweeeesweeeswne\neneneneeswnwswneenenwneswnenesweeswee\nwnwnesenwwwnwnenwswwnwwnwwseswnwenw\nwwnwwwwwwwwswwwsew\nenwneneneneneneswneeswseneeenwneene\nsesesesesenwseseeseseseswneseswwswswsenee\nswneswnwneenesweeneswsweneeneenenwne\nnwnwnenwnwswnwnwnwsenwnwnenwsenwwsenenese\nneeneseswseeneweweneweeseeneenw\nwwswwswnwswneswswwnwseswwwnwswewse\nwswwswwswswswwweswsww\nswswswnwneseswswsewswswseseneswnwseeswswse\nnwwwwnwewwwnwwww\nswseenenwewnweswsenwswwseseesesese\nsesewneseseseseseenwneweswseenwswsese\nnenwnwnenenwswnwnenwnwnenwswneenwseene\nneswwswsweneswswwswseswswwnewnesww\nswswseseeseesenwsesesenwnwneseeseese\nwwnenwnwnwwwwnwsenwwwseweesenw\nnwenenwnwsewnwnwnwneewnwwswswww\nnenwsenwswnenwnwswnwneneswnwnwenwnenwnw\nneneneenenweseewnenene\nnwwswsenwseseseeenwnwsesenwenwesese\nswswswswswswswswswswneswsewswse\nswnwnwnwnesenenewnwnwnwnwnwnee\nnwswsenesewwswswswneswswswseseseswwe\nswsweswwwneswenwnwewwwwese\newnewseeseseewseesewseneeenwese\nneswenwseeeneswnwwnwwnwseewwswnw\nnwsenwsesenwnwnewneswneseneneeenenesw\nnenweswnwneswnwwwnwswnwsenenenwnwnwsww\neeeeeweswseseseenwnweeneesee\nseseseseswwneswseswsesesese\nswwswswswwwnwwwwswwnewwseeswnw\neseseswswseswnwseseswsweswswwseweswse\nnwnwnwnwnwwenwwseswnww\nneneneneneenwnenenenenwswnene\nsesewewnenwwnwsenwswnene\nswsenwsweneneenewewneenwwneewne\nswnesesesenwseneswnwnenenwnenwswnee\nseeswnwnwsenenweneneneneneneneneenew\nwneewsesewsesenenwsenewsewwsenenese\nnwenwswnwseeswseneseswneseswswseswseswsesw\nwsweswneswswswswswswswswswswnweswswsw\nseseeeseeeseewse\nswnenenwsenwnenwnesewneewneesesenwnwnwne\nnwenwsewnwnwenenwnenwnwswnwwwnwnwswe\nnwenwswseswwswseseswsweswswswswseswswne\nwsweeeneewseweneswnenenwneenenenene\nswwswswneseseswseswswswswseswneswswsw\neweswswnenesenwswenwseneneswneswswnene\nnwewwnwnwnwenenwswnwsewnwnwswnwnwnw\nneeneneneneeswnwneeeeeneweenewne\nseseeseeneseseeseseweseswnwsesesese\neeeeewewee\nswseneseeenwseewnwseswnenwswnwwnww\nseseneswswsesenwswseseswseseswesese\nnenenewwswseneeswneswsenenwnesew\nnesenenenwnenwnenenwnewenenwnwnewene\neseseseeeweeneseeeseswsese\nenwwwwsewwwnenwswsenwnwnwnwnewnwwnw\nnwwswwswwnenwwwwwwnwneseewsee\nneneswnenenenenwnenenenesenewenenenene\nwwwneweswwwswwwwwneswswwsww\nneneneneneeenenwneneswnenenenene\nseseseseswseseseseseneseseeneswenwswsese\neeeswneswseeseswswseeneeneeenwe\nnenenwnwnesesenwnenenwnwnwenwswnwwwnwne\nnwseseseeeseseseeeswnwsese\neseseeseenwwseweseseesewseseseee\nenwneswwsenenenenenenenwwnenesenenenenene\nnweweswnwwnewwnwnwwnwwnwwswwe\nwneswswneswswswsewwwswwnwsewswww\nnwenwswnwnweswseswsweswswswswsw\nseseseewsenwswnweswsenwew\nswwwenwnwnwwwwwwnewwnwnwsewwew\nswnwwsweswsenwsenewenewneseeenese\nnesenwnwnwnwnenwnwnwnwnwnwsese\nnenwneenenenenenenenewnenwseswswnenene\nenweneesesweeeeswenwenwenweeesw\nwswnwnewwwwswwwnwwseesesww\nnwwwnenwnwsewewswnweswww\nswswswswnenweswswswswswswneswswswneswsw\nnwsewswswswswswswnwweswswswswswswswnesw\nsenwnwnwsenwwwwnwnwnenwnwnw\nsenenewenwsesenesewswsewneseseswswnewne\nwnwwnwenwwwwnwwnwwwnwnw\nnweseenwseseseneseesesesenwsesesewsw\nseseeeseseswnwsesese\nwwwwnwnwewwwwwswsewwnewww\nneneeeeeeeeweeene\neneneneneneswsenenenweneseeenenwnewne\nwesenewseswswswseewswnesewseneswsw\neseeseesesewseswsesesesewnwsesesee\nnweenwseseeseneeenewsesewswwse\nnwnesenwnenwwnwneenwnwnenenenwnwswnwne\nseseseseseswnenwsesesesesesesesesesenesw\nneneneneneenenewneneneneneneneswnwwe\nnwwswnwnenwnwnwsew\nswwnewwwnwneenwwwwnwsewswwww\nswswwnwswswwswwswswswse\nnenenwnwnwneeneneenewnenwnwsenwnwswsw\nswswswneewsenewswnw\nnewneeeneseeenenwneeeeewneee\nseswseswnwsewseseswsesesenwswseneseesesesw\nnenwwnweeswnenwsenenenwwesesenww\nnwswseswneswswswswswnwswse\nweseeeseseweeseeseseenenwseseee\nnwwwwswewswwswsewnwnwnenewewse\nwwswnewwwwwwwwwwww\nwnwwnwewswswesenwewnwwwnwnwwwne\nnenwnwnwnewwnwnwwswnwnwenwnwnwnwsenw\nnenwnwneswseswswneeneeswnwnwswnwseswnenw\nsenwwseneneeseseesenwwseeswseeeseene\nsenwneenwseenwnwwwneneneenwnwnwnwsew\nsesweseseswnwswswnwswseseseseeswseseww\nnwwwnwwenwnwnwnwnwnwnwsewsenwwswew\nswneswwsewsewseeswswnenwswwwneswnenw\nenweswneeseswsesweenwneswnwneenwewe\newwswnwwwwwwwnwwenwnwnwwnw\nnenenwnenwnwnwnwnwnwse\nswnwsweneswneeesenwswewnwneseeesw\neewnwnwseswenwswsewnesesesenwnese\nnwnenesenesenwwnenwnwnwnwenwnwwnwswnwnw\nnwseswnwesenwnesee\nnewseswsenwnwsewswneneenewnwwnwne\neneneeewenenenwneeeenesw\nwswswswwneswswwswwsw\nwswseseswwnwnweenw\nwwwwwseswwwenesw\nneenenwenenenwnesenwneenenenwwswnesw\nswswneswenwswswswwswsesweeswswswsenw\nsesenenwsenwnenwneswnwnwwswnwnewnewnw\nnwewwnwnwwwnwswnwenwenwswewnww\nneeneneneeswneneneneswnenwnenenw\nnesewewewsesenenenweenenesweswsenw\nwnwwseswnwwswnweneswewnwnwnwwnwe\neeswnweeeeseeseeewneeeenweee\nwswwswsenewsesewewnewsenwnwwwnw\nwswesenwnwneswnwe\nwwwwwnewwwwswnewwsenwnwnw\nswswsesesesesesenwesesenwse\nnwnwnwnwswnenwwsenwnwnwwwnenwnwsewnwnw\nneswswswseswneswseseswnwwswswnwneswswswse\nwswwwnwnwwnwwsewnenwwwneswwwne\nseesweeeenesenweeeweeeenwse\nsenwnwnwnwsenwneneswenenwwnenenenenwnenw\nswneneswwneeswwswwsenwwswnwneswsesw\nneeswswsweeeneesweeeneeeneeswnw\nnwnwseesenwwnwnwsesenwneenwnwnwwnwne\nsewsenwwsesewwseeseeswseeeseesese\nswneswswswswswnweswswneswswswswsww\nswneswswswseseswswswwswneseseswswswsenesw\neswnwwwswneesewwswwwnwwwwswse\nnwnenwnwnwnwnwswsenwnwnwswnenwnwnwnwenw\neenenwswnwneswsenwswnwnenwswwnenenwsw\nsenwswseseseeeeenw\nneneneneneswnwnwwnenwnesenweneswnwnese\nswnwwwswswswwwwseww\nswsesenwswswnweseseneswswswsesesewswsw\nnweeeeeneeeseene\nesesewswsesesewnwsewseewnenwnesene\nneenwseeweeweswswnwese\nwnenwnweseneeesenewseeswww\nnwnwnwnwnwnwnwwnenwnwnwwnwse\nnwnwseswnwnwwwnwnwnwnenwsenwnwwnenenwnw\nneewswnenwseseswwnwseseeeseesenweese\nseswswseswseseswewswwswneseswswseeswnw\neeeeeesenwswswenenwneeeewsweswnw"
  println(countBlack(input))
  println(countBlackAfter(input, 100))
}

class Tile(val col: Int, val row: Int, var color: String = "W") {
  def get(dir: String): (Int, Int) = {
    dir match {
      case "e" => (col + 1, row)
      case "w" => (col - 1, row)
      case "se" => if (row % 2 == 0) (col, row + 1) else (col + 1, row + 1)
      case "ne" => if (row % 2 == 0) (col, row - 1) else (col + 1, row - 1)
      case "sw" => if (row % 2 == 0) (col - 1, row + 1) else (col, row + 1)
      case "nw" => if (row % 2 == 0) (col - 1, row - 1) else (col, row - 1)
    }
  }

  def neighbours(): List[(Int, Int)] = {
    List(
      this.get("e"),
      this.get("w"),
      this.get("se"),
      this.get("ne"),
      this.get("sw"),
      this.get("nw")
    )
  }
}
