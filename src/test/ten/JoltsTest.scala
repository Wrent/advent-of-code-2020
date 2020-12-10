package ten

import org.scalatest.funsuite.AnyFunSuite

class JoltsTest extends AnyFunSuite {
  test("jolts") {
    assert(Jolts.countDifferences("16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4") === 35)
    assert(Jolts.countDifferences("28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3") === 220)
  }

  test("jolts 2") {
    assert(Jolts.countPossibilities("16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4").toInt === 8)
    assert(Jolts.countPossibilities("28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3").toInt === 19208)
  }

  test("adapter") {
    val adapters = Jolts.parseAdapters("16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
    assert(adapters(12).countPossibilities(adapters) === 0)
    assert(adapters(11).countPossibilities(adapters) === 1)
    assert(adapters(10).countPossibilities(adapters) === 1)
    assert(adapters(9).countPossibilities(adapters) === 1)
    assert(adapters(8).countPossibilities(adapters) === 1)
  }


}
