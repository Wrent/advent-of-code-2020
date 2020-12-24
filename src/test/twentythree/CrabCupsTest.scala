package twentythree

import org.scalatest.funsuite.AnyFunSuite

class CrabCupsTest extends AnyFunSuite {
  test("crab cups") {
//    assert(CrabCups.getLabels("389125467", 1) === "54673289")
//    assert(CrabCups.getLabels("389125467", 2) === "32546789")
//    assert(CrabCups.getLabels("389125467", 3) === "34672589")
    assert(CrabCups.getLabels("389125467", 10) === "92658374")
    assert(CrabCups.getLabels("389125467", 100) === "67384529")
  }

  test("crab cups 2") {
    assert(CrabCups.getCorrectLabels("389125467", 10, 9) === 18)
    assert(CrabCups.getCorrectLabels("389125467", 100, 9) === 42)
    assert(CrabCups.getCorrectLabels("389125467", 10000000, 1000000) === 149245887792L)
  }

}
