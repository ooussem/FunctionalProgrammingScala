package com.book.fpinscala.chap4

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class EitherTest extends AnyFunSuite {

  test("map() Either Right test") {
    val eitherTest: Either[Exception, Int] = Right(4)
    val rightExpected: Either[Exception, Int] = Right(8)

    eitherTest.map(_ * 2) shouldEqual rightExpected
  }

  test("map() Either Left test") {
    val eitherTest: Either[Exception, Int] = Left(new Exception("Left !!"))
    val leftExpected: Either[Exception, Int] = Left(new Exception("Left !!"))

    eitherTest.map(_ * 2) shouldEqual leftExpected
  }


  test("flatMap() Either Right test") {
    val eitherTest: Either[Exception, Int] = Right(4)
    val functionForTest: Int => Either[Exception, Int] = i => Right(i * 2)

    val rightExpected: Either[Exception, Int] = Right(8)

    eitherTest.flatMap(functionForTest) shouldEqual rightExpected
  }


  test("map2() Either Right test") {
    val eitherTest: Either[Exception, Int] = Right(4)
    val otherEither: Either[Exception, String] = Right("4")
    val functionForTest: (Int, String) => Double = (i, y) => i + y.toDouble

    val rightExpected: Either[Exception, Double] = Right(8)

    eitherTest.map2(otherEither)(functionForTest) shouldEqual rightExpected
  }

}
