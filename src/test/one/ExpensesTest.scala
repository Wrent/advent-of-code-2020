package one

import org.scalatest.funsuite.AnyFunSuite

class ExpensesTest extends AnyFunSuite {
  test("expenses") {
    assert(Expenses.calculate("1721\n979\n366\n299\n675\n1456") === 514579)
  }

  test("expenses part two") {
    assert(Expenses.calculateThree("1721\n979\n366\n299\n675\n1456") === 241861950)
  }

}
