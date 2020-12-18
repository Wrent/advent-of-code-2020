package eighteen

import org.scalatest.funsuite.AnyFunSuite

class OrderOfOperationsTest extends AnyFunSuite {
  test("order of operations") {
    assert(OrderOfOperations.sumExpressions("1 + 2 * 3 + 4 * 5 + 6") === 71)
    assert(OrderOfOperations.sumExpressions("1 + (2 * 3) + (4 * (5 + 6))") === 51)
    assert(OrderOfOperations.sumExpressions("2 * 3 + (4 * 5)") === 26)
    assert(OrderOfOperations.sumExpressions("5 + (8 * 3 + 9 + 3 * 4 * 3)") === 437)
    assert(OrderOfOperations.sumExpressions("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") === 12240)
    assert(OrderOfOperations.sumExpressions("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") === 13632)
    assert(OrderOfOperations.sumExpressions("2 * 3 + (4 * 5)\n5 + (8 * 3 + 9 + 3 * 4 * 3)") === 26 + 437)
  }

  test("order of operations 2") {
    assert(OrderOfOperations.sumAdvancedExpressions("1 + 2 * 3 + 4 * 5 + 6") === 231)
    assert(OrderOfOperations.sumAdvancedExpressions("1 + (2 * 3) + (4 * (5 + 6))") === 51)
    assert(OrderOfOperations.sumAdvancedExpressions("2 * 3 + (4 * 5)") === 46)
    assert(OrderOfOperations.sumAdvancedExpressions("5 + (8 * 3 + 9 + 3 * 4 * 3)") === 1445)
    assert(OrderOfOperations.sumAdvancedExpressions("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") === 669060)
    assert(OrderOfOperations.sumAdvancedExpressions("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") === 23340)
    assert(OrderOfOperations.sumAdvancedExpressions("2 * 3 + (4 * 5)\n5 + (8 * 3 + 9 + 3 * 4 * 3)") === 46 + 1445)
  }
}
