package two

import org.scalatest.funsuite.AnyFunSuite

class PasswordsTest extends AnyFunSuite {
  test("passwords") {
    assert(Passwords.countPasswords("1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc") === 2)
  }

  test("passwords two") {
    assert(Passwords.countPasswords("1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc", TOBOGGAN()) === 1)
  }

}
