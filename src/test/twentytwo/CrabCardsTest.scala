package twentytwo

import org.scalatest.funsuite.AnyFunSuite

class CrabCardsTest extends AnyFunSuite {
  test("crab cards") {
    assert(CrabCards.countWinnerScore("Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10") === 306)
  }

  test("crab cards 2") {
    assert(CrabCards.countRecursiveScore("Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10") === 291)
  }
}
