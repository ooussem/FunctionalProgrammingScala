package com.book.fpinscala.chap3

import org.scalatest.funsuite.AnyFunSuite
import List._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ListTest extends AnyFunSuite {

  test("Sum") {
    val listTest: List[Int] = List(1, 2, 3)
    val expected = 6

    sumTailRel(listTest, 0) shouldEqual expected
  }

}
