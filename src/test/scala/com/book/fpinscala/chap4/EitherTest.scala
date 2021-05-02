package com.book.fpinscala.chap4

import com.book.fpinscala.chap4.EitherComp.{Age, Name, Person, mkPerson, mkPersonWithMultiLeft}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class EitherTest extends AnyFunSuite {

  test("map() Either Right test") {
    val eitherTest: Either[Exception, Int] = Right(4)
    val rightExpected: Either[Exception, Int] = Right(8)

    eitherTest.map(_ * 2) shouldEqual rightExpected
  }

  test("map() Either Left test") {
    val exceptionTest = new Exception("Left !!")
    val eitherTest: Either[Exception, Int]    = Left(exceptionTest)
    val leftExpected: Either[Exception, Int]  = Left(exceptionTest)

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

  test("sequence() Either test w/o error") {
    val eitherTest: List[Either[Exception, Int]] = List(Right(4), Right(8))
    val expectedResult: Either[Exception, List[Int]] = Right(List(4, 8))

    EitherComp.sequence(eitherTest) shouldEqual expectedResult
  }

  test("traverse()") {
    val eitherTest: List[Int] = List(4, 8)
    val functionTest: Int => Either[Exception, String] = i => Right(i.toString)

    val expectedResult: Either[Exception, List[String]] = Right(List("4", "8"))

    EitherComp.traverse(eitherTest)(functionTest) shouldEqual expectedResult
  }

  test("mkPerson() test pass") {
    val expectedResult: Either[String, Person] = Right(Person(Name("toto"), Age(28)))

    mkPerson("toto", 28) shouldEqual expectedResult
  }

  test("mkPerson() test fail name") {
    mkPerson("", 28) shouldEqual Left("Name is empty.")
  }

  test("mkPerson() test fail name and age but with the first Left") {
    mkPerson("", -5) shouldEqual Left("Name is empty.")
  }

  test("mkPerson() test fail age") {
    mkPerson("toto", -5) shouldEqual Left("Age is out of range.")
  }

  test("mkPersonWithMultiLeft() test success") {
    mkPersonWithMultiLeft("", -5)(Person(Name("X"), Age(-1))) shouldEqual MultiLeft(Seq("Name is empty.", "Age is out of range."))
  }

}
