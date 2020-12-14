package fourteen

import org.scalatest.funsuite.AnyFunSuite

class DockingTest extends AnyFunSuite {
  test("docking") {
    assert(Docking.sumMemory("mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0") === 165)
  }

  test("docking floating") {
    assert(Docking.sumMemoryFloating("mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1") === 208)
  }

}
