package com.book.fpinscala.chap4

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class OptionTest extends AnyFunSuite {
  test("map test ") {
    val funcToTest: Int => Int => Int = x => y => x + y
    def f2(x: Int, y: Int): Int = x + y
    val resExpected: Option[Int] = Some(3)

    val numberOpt: Option[Int] = Some(1)

    val result = numberOpt.map(f2(_,2))
    result shouldEqual resExpected
  }

  test("flatMap_2 test") {
    val funcToTest: Int => Int => Option[Int] = x => y => Some(x + y)

    val resExpected: Option[Int] = Some(3)

    val numberOpt: Option[Int] = Some(1)

    val result = numberOpt.flatMap(funcToTest(2))
    result shouldEqual resExpected
  }

}
