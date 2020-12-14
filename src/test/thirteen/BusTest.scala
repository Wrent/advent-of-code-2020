package thirteen

import org.scalatest.funsuite.AnyFunSuite

class BusTest extends AnyFunSuite {
  test("bus") {
    assert(Bus.findBus("939\n7,13,x,x,59,x,31,19") === 295)
  }

  test("bus 2") {
    assert(Bus.findPattern("939\n17,x,13,19", 0) === 3417)
//    assert(Bus.findPattern("939\n7,13,x,x,59,x,31,19", 100000) === 1068781)
//    assert(Bus.findPattern("1\n67,7,59,61", 750000) === 754018)
//    assert(Bus.findPattern("1\n67,x,7,59,61", 750000) === 779210)
//    assert(Bus.findPattern("1\n67,7,x,59,61", 1250000) === 1261476)
//    assert(Bus.findPattern("1\n1789,37,47,1889", 1202161000) === 1202161486)
  }

  test("bus clever") {
    assert(Bus.findPatternClever("939\n17,x,13,19") === 3417)
    assert(Bus.findPatternClever("939\n7,13,x,x,59,x,31,19") === 1068781)
    assert(Bus.findPatternClever("1\n67,7,59,61") === 754018)
    assert(Bus.findPatternClever("1\n67,x,7,59,61") === 779210)
    assert(Bus.findPatternClever("1\n67,7,x,59,61") === 1261476)
    assert(Bus.findPatternClever("1\n1789,37,47,1889") === 1202161486)
  }

  test("bus chinese") {
//    assert(Bus.findPatternChinese("939\n3,x,x,4,5") === 39)
    assert(Bus.findPatternChinese("939\n17,x,13,19") === 3417)
    assert(Bus.findPatternChinese("939\n7,13,x,x,59,x,31,19") === 1068781)
    assert(Bus.findPatternChinese("1\n67,7,59,61") === 754018)
    assert(Bus.findPatternChinese("1\n67,x,7,59,61") === 779210)
    assert(Bus.findPatternChinese("1\n67,7,x,59,61") === 1261476)
    assert(Bus.findPatternChinese("1\n1789,37,47,1889") === 1202161486)
  }

}
