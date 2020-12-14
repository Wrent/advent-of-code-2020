package twelve

import org.scalatest.funsuite.AnyFunSuite

class FerryTest extends AnyFunSuite {
  test("ferry") {
    assert(Ferry.getFinalSpot("F10\nN3\nF7\nR90\nF11") === 25)
  }

  test("ferry 2") {
    assert(Ferry.getWaypointSpot("F10\nN3\nF7\nR90\nF11") === 286)
  }
}
