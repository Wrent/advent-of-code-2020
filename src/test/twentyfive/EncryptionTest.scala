package twentyfive

import org.scalatest.funsuite.AnyFunSuite

class EncryptionTest extends AnyFunSuite {
  test("encryption") {
    assert(Encryption.getEncryptionKey(5764801, 17807724) === 14897079)
  }

  test("loop size") {
    assert(Encryption.getLoopSize(5764801) === 8)
    assert(Encryption.getLoopSize(17807724) === 11)
  }
}
