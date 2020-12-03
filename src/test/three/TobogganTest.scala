package three

import org.scalatest.funsuite.AnyFunSuite

class TobogganTest extends AnyFunSuite {
  test("toboggan") {
    assert(Toboggan.countTrees("..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#", 3, 1) === 7)
  }

  test("toboggan second") {
    assert(Toboggan.countMultipleSlopes("..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#") === 336)
  }

}
