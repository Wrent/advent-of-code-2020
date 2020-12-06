package five

import org.scalatest.funsuite.AnyFunSuite

class BoardingTest extends AnyFunSuite {
  test("boarding") {
    assert(Boarding.parseSeat("FBFBBFFRLR") === (44, 5))
    assert(Boarding.parseSeat("BFFFBBFRRR") === (70, 7))
    assert(Boarding.getSeatId("BFFFBBFRRR") === 567)
    assert(Boarding.parseSeat("FFFBBBFRRR") === (14, 7))
    assert(Boarding.getSeatId("FFFBBBFRRR") === 119)
    assert(Boarding.parseSeat("BBFFBBFRLL") === (102, 4))
    assert(Boarding.getSeatId("BBFFBBFRLL") === 820)
  }

}
