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

    streamToTest.takeWhile(func).toList.reverse shouldEqual expected.toList
  }


  test("takeWhileGitHubSol() with result") {
    val streamToTest = Stream.apply(2, 4, 5, 6)
    val func: Int => Boolean = x => x % 2 == 0

    val expected = Stream.apply(2, 4)

    streamToTest.takeWhileGitHubSol(func).toList shouldEqual expected.toList
  }

  test("takeWhile() with all result") {
    val streamToTest = Stream.apply(2, 4, 6)
    val func: Int => Boolean = x => x % 2 == 0

    val expected = Stream.apply(2, 4, 6)

    streamToTest.takeWhile(func).toList.reverse shouldEqual expected.toList
  }

  test("exist() test") {
    val streamToTest = Stream.apply(1, 2)
    val func: Int => Boolean = x => x % 2 == 0

    val expected = true

    streamToTest.exist(func) shouldEqual expected
  }



  test("takeWhileWithFoldRight() empty result") {
    val streamToTest = Stream.apply(1, 2, 3, 4, 5)
    val func: Int => Boolean = x => x % 2 == 0

    val expected = Empty

    streamToTest.takeWhileWithFoldRight(func) shouldEqual expected
  }


  test("takeWhileWithFoldRight() with result") {
    val streamToTest = Stream.apply(2, 4, 5, 6)
    val func: Int => Boolean = x => x % 2 == 0

    val expected = Stream.apply(2, 4)

    streamToTest.takeWhileWithFoldRight(func).toList shouldEqual expected.toList
  }

  test("headOptionWithFoldRight() None result for an empty stream") {
    val streamToTest = Stream.apply()
    val expected: Option[Int] = None
    streamToTest.headOptionWithFoldRight shouldEqual expected
  }


  test("headOptionWithFoldRight() with Some(1) ") {
    val streamToTest = Stream.apply(1, 2)
    val expected: Option[Int] = Some(1)
    streamToTest.headOptionWithFoldRight shouldEqual expected
  }

  test("[exo 5.7] test map() with foldRight") {
    val streamToTest = Stream.apply(1, 2)
    val streamExpected = Stream.apply("1", "2")
    streamToTest.map(e => e.toString).toList shouldEqual streamExpected.toList
  }

  test("[exo 5.7] test map() with foldRight with empty stream") {
    val streamToTest = Stream.empty
    val streamExpected = Stream.empty
    streamToTest.map(e => e.toString).toList shouldEqual streamExpected.toList
  }

  test("[exo 5.7] test filter() with foldRight normal case") {
    val streamToTest = Stream.apply(1, 2)
    val streamExpected = Stream.apply(2)
    streamToTest.filter(e => e % 2 == 0).toList shouldEqual streamExpected.toList
  }

  test("[exo 5.7] test filter() with foldRight with empty case") {
    val streamToTest = Stream.apply(1, 3)
    val streamExpected = Stream.empty
    streamToTest.filter(e => e % 2 == 0).toList shouldEqual streamExpected.toList
  }

  test("[exo 5.7] test appendElement() with foldRight normal case") {
    val toAppend = 2
    val myStream = Stream.apply(1)
    val streamExpected = Stream.apply(2, 1)
    myStream.appendElement(toAppend).toList shouldEqual streamExpected.toList
  }

  test("[exo 5.7] test appendElement() with foldRight with empty stream") {
    val toAppend = 2
    val myStream = Stream.empty
    val streamExpected = Stream.apply(2)
    myStream.appendElement(toAppend).toList shouldEqual streamExpected.toList
  }

  test("[exo 5.7] test append() with foldRight normal case") {
    val streamToAppend = Stream.cons(2, Stream.empty)
    val myStream = Stream.apply(1)
    val streamExpected = Stream.apply(1, 2)
    myStream.append(streamToAppend).toList shouldEqual streamExpected.toList
  }

  test("[exo 5.7] test flatMap() with foldRight with normal case") {
    val streamToTest = Stream.apply(1, 2)
    val streamExpected = Stream.apply("1", "2")
    val funcToTry: Int => Stream[String] = x => Stream.cons(x.toString, Stream.empty)
    streamToTest.flatMap(funcToTry).toList shouldEqual streamExpected.toList
  }

  test("[exo 5.7] test flatMap() with foldRight with empty case") {
    val streamToTest = Stream.empty
    val streamExpected = Stream.empty
    val funcToTry: Int => Stream[String] = x => Stream.cons(x.toString, Stream.empty)
    streamToTest.flatMap(funcToTry).toList shouldEqual streamExpected.toList
  }

  test("test find() with filter") {
    val myStream = Stream.apply(1, 2)
    val expected = Option(2)
    myStream.find(_ % 2 == 0) shouldEqual expected
  }

  test("test fibs()") {
    val expectedStream = Stream(0, 1, 1, 2, 3, 5, 8, 13)
    Stream.fibs(Stream(0, 1)).take(8).toList shouldEqual expectedStream.toList
  }

  test("test fibsSolution()") {
    val expectedStream = Stream(0, 1, 1, 2, 3, 5, 8, 13)
    Stream.fibsSolution.take(8).toList shouldEqual expectedStream.toList
  }

  test("test next Fibo number") {
    def nextF(stream: Stream[Int], beforeLast: Int = 0, acc: Int = 1): Int = {
      stream match {
        case Empty => acc - beforeLast
        case Cons(h, t) => nextF(t(), h(), h() + acc)
      }
    }

    nextF(Stream(0, 1)) shouldEqual 1
    nextF(Stream(0, 1, 1)) shouldEqual 2
    nextF(Stream(0, 1, 1, 2)) shouldEqual 3
    nextF(Stream(0, 1, 1, 2, 3)) shouldEqual 5
    nextF(Stream(0, 1, 1, 2, 3, 5)) shouldEqual 8
    nextF(Stream(0, 1, 1, 2, 3, 5, 8)) shouldEqual 13
  }



}

