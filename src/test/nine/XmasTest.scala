package nine

import org.scalatest.funsuite.AnyFunSuite

class XmasTest extends AnyFunSuite {
  test("xmas") {
    assert(Xmas.findInvalidNumber("20\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n1\n21\n22\n23\n24\n25\n26\n100", 25) === 100)
    assert(Xmas.findInvalidNumber("20\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n1\n21\n22\n23\n24\n25\n49\n100", 25) === 100)
    assert(Xmas.findInvalidNumber("20\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n1\n21\n22\n23\n24\n25\n50\n100", 25) === 50)
    assert(Xmas.findInvalidNumber("20\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n1\n21\n22\n23\n24\n25\n45\n26\n100", 25) === 100)
    assert(Xmas.findInvalidNumber("20\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n1\n21\n22\n23\n24\n25\n45\n65\n100", 25) === 65)
    assert(Xmas.findInvalidNumber("20\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n1\n21\n22\n23\n24\n25\n45\n64\n100", 25) === 100)
    assert(Xmas.findInvalidNumber("20\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n1\n21\n22\n23\n24\n25\n45\n66\n100", 25) === 100)
    assert(Xmas.findInvalidNumber("35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576", 5) === 127)
  }

  test("xmas 2") {
    assert(Xmas.findWeakness("35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576", 127) === 62)
  }

}
