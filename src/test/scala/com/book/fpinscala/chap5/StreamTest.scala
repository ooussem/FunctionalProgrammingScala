package com.book.fpinscala.chap5

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class StreamTest extends AnyFunSuite {

  test("takeWhile() empty result") {
    val streamToTest = Stream.apply(1, 2, 3, 4, 5)
    val func: Int => Boolean = x => x % 2 == 0

    val expected = Empty

    streamToTest.takeWhile(func) shouldEqual expected
  }


  test("takeWhile() with result") {
    val streamToTest = Stream.apply(2, 4, 5, 6)
    val func: Int => Boolean = x => x % 2 == 0

    val expected = Stream.apply(2, 4)

    streamToTest.takeWhile(func).toList shouldEqual expected.toList
  }

  test("takeWhile() with all result") {
    val streamToTest = Stream.apply(2, 4, 6)
    val func: Int => Boolean = x => x % 2 == 0

    val expected = Stream.apply(2, 4, 6)

    streamToTest.takeWhile(func).toList shouldEqual expected.toList
  }


}

