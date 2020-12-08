package eight

import one.Expenses
import org.scalatest.funsuite.AnyFunSuite

class GameConsoleTest extends AnyFunSuite {
  test("console") {
    assert(GameConsole.getAccBeforeRepeat("nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6") === 5)
  }

  test("console2") {
    assert(GameConsole.getAccAfterFix("nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6") === 8)
  }
}
