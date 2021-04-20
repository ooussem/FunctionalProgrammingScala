package com.book.fpinscala.chap4

import com.book.fpinscala.chap4.None.{salaryNetByMonthOpt, variance}
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

  test ("Variance") {
    val seqTest: Seq[Double] = Seq(1, 3, 9, 4, 5, 6, 4)
    println(variance(seqTest))
  }


  test("lift method") {
    val sal = 55000
    salaryNetByMonthOpt(Some(sal)) shouldEqual Some(3437.5)
  }

  test("sequence method") {

    val listToTest: List[Option[Double]] = List(Some(4), Some(6), Some(90), Some(10), Some(3))
    val expected: Option[List[Double]] = Some(List(4, 6, 90, 10, 3))

    Option.WithScalaList.sequenceWithFold(listToTest) shouldEqual expected
    Option.WithScalaList.sequence(listToTest) shouldEqual expected
  }

  test("sequence method test with None") {

    val listToTest: List[Option[Double]] = List(Some(4), None, Some(90), Some(10), Some(3))
    val expected: Option[List[Double]] = None

    Option.WithScalaList.sequence(listToTest) shouldEqual expected
  }

}
