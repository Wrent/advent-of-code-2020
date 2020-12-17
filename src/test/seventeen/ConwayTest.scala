package seventeen

import org.scalatest.funsuite.AnyFunSuite

class ConwayTest extends AnyFunSuite {
  test("conway") {
    assert(Conway.getActiveCubes(".#.\n..#\n###", 1) === 11)
    assert(Conway.getActiveCubes(".#.\n..#\n###", 6) === 112)
  }

  test("neighbours") {
    assert(Coord3D(0, 0, 0).neighbours().size === 26)
  }

  test("neighbours2") {
    assert(Coord4D(0, 0, 0, 0).neighbours().size === 80)
  }

  test("conway2") {
    assert(Conway.getActiveCubes4d(".#.\n..#\n###", 6) === 848)
  }
}
