package cz.wrent.aoc20.hello

import org.scalatest.funsuite.AnyFunSuite

class HelloTest extends AnyFunSuite {
  test("Hello.square") {
    assert(Hello.square(3) === 9)
  }
}
