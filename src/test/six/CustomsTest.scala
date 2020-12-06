package six

import org.scalatest.funsuite.AnyFunSuite

class CustomsTest extends AnyFunSuite {
  test("customs") {
    assert(Customs.sumCounts("abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb") === 11)
  }

  test("customs 2") {
    assert(Customs.sumCountsAll("abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb") === 6)
  }
}
