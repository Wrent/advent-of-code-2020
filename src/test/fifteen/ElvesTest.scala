package fifteen

import org.scalatest.funsuite.AnyFunSuite

class ElvesTest extends AnyFunSuite {
  test("elves") {
    assert(Elves.getNth("0,3,6", 4) === 0)
    assert(Elves.getNth("0,3,6", 5) === 3)
    assert(Elves.getNth("0,3,6", 2020) === 436)
    assert(Elves.getNth("1,3,2", 2020) === 1)
    assert(Elves.getNth("2,1,3", 2020) === 10)
    assert(Elves.getNth("1,2,3", 2020) === 27)
    assert(Elves.getNth("2,3,1", 2020) === 78)
    assert(Elves.getNth("3,2,1", 2020) === 438)
    assert(Elves.getNth("3,1,2", 2020) === 1836)
  }

}
