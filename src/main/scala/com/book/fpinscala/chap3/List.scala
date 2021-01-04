package com.book.fpinscala.chap3

import com.book.fpinscala.chap3.List.{appendWithFoldLeft, appendWithFoldRight, foldLeft, foldRight, length, map, reverseOrderList, reverseOrderListWithFold}

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }


  def products(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x, xs) => x *
      products(xs)
  }

  def products2(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(x, xs) => x * products(xs)
  }


  def tail[T](list: List[T]): List[T] = {
    list match {
      case Nil => Nil // or exception
      case Cons(_, t) => t
    }
  }

  def setHead[T](newHead: T, list: List[T]): List[T] = {
    list match {
      case Nil => Nil // or exception
      case Cons(_, t) => Cons(newHead, t)
    }
  }

  def drop0[T](numToDrop: Int, list: List[T]): List[T] = {
    @tailrec
    def go(i: Int, newList: List[T]): List[T] = {
      if (i == numToDrop) newList
      else {
        newList match {
          case Cons(_, t) => go(i + 1, t)
        }
      }
    }

    go(1, list)
  }


  @tailrec
  def drop2[T](n: Int, list: List[T]): List[T] = {
    if (n <= 0) list
    else {
      list match {
        case Nil => Nil
        case Cons(_, t) => drop2(n - 1, t)
      }
    }
  }


  @tailrec
  def dropWhile[T](list: List[T])(f: T => Boolean): List[T] = {
    list match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => list
    }
  }


  def add[T](xs: List[T], x: T): List[T] = {
    xs match {
      case Nil => Cons(x, Nil)
      case Cons(a, as) => Cons(x, Cons(a, as))
    }
  }


  /*
      l1 = Cons(1, Nil)
      l2 = Cons(2, Nil)
      lf = Cons(1, Cons(2, Nil))
   */
  def append[T](l1: List[T], l2: List[T]): List[T] = {
    l1 match {
      case Nil => l2
      case Cons(x, xs) => Cons(x, append(xs, l2))
    }
  }

  def init[T](list: List[T]): List[T] = {
    list match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  /*
   * 3.4 Recursion over lists and generalizing to higher order functions
   *
   * def sum(ints: List[Int]): Int = ints match {
   *  case Nil => 0
   *  case Cons(x,xs) => x + sum(xs)
   * }
   * def product(ds: List[Double]): Double = ds match {
   *  case Nil => 1.0
   *  case Cons(x, xs) => x * product(xs)
   * }
   *
   * turn the subexpression into a function that accepts these variables as arguments.
   *
   * Giving a list: Cons(2, 3, Nil) and a function f: (x, y) -> x + y, we want =>
   *    f(2, foldRight(3, 0)(f)) => 2 + f(3, foldRight(Nil, 0)(f)) => 2 + (3 + foldRight(Nil, 0)(f)) => 2 + 3 + 0 => 5
   *
   */
  def foldRight[A, B](list: List[A], defaultValue: B)(f: (A, B) => B): B = {
    list match {
      case Nil => defaultValue
      case Cons(x, xs) => f(x, foldRight(xs, defaultValue)(f))
    }
  }

  // 3.7 => Il faut une évaluation complète de chaque expression jusqu'à la fin. Il faut évaluer les expressions de manière lazy

  // 3.8 => Résultat: foldRight(listTest, Nil: List[Int])(Cons(_, _)) = Cons(2,Cons(3,Cons(5,Cons(6,Cons(10,Cons(5,Nil)))))).

  // 3.9 Compute the length of a list using foldRight.
  def length[A](list: List[A]): Int = {
    foldRight(list, 0)((_, y) => 1 + y)
  }

  // 3.10: foldLeft
  @tailrec
  def foldLeft[A, B](list: List[A], defaultValue: B)(f: (B, A) => B): B = {
    list match {
      case Nil => defaultValue
      case Cons(x, xs) => foldLeft(xs, f(defaultValue, x))(f)
    }
  }


  /*
      given Cons(1, Cons(2, Cons(3, Nil) ) ) => reverseOrderList(Cons(1, Cons(2, Cons(3, Nil))), defaultList) => reverseOrderList(Cons(2, Cons(3, Nil)), Cons(1, defaultList)) =>
        reverseOrderList(Cons(3, Nil), Cons(2 , Cons(1, defaultList)) => Cons(3, Cons(2, Cons(1, defaultList)))
   */
  // exo 3.12
  @tailrec
  def reverseOrderList[A](list: List[A], defaultList: List[A]): List[A] = {
    list match {
      case Cons(x, xs) => reverseOrderList(xs, Cons(x, defaultList))
      case Nil => defaultList
    }
  }

  def reverseOrderListWithFold[A](list: List[A]): List[A] = {
    foldLeft(list, List[A]())((acc, head) => Cons(head, acc))
  }

  // 3.13 Hard


  // 3.14
  def appendWithFoldLeft[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(l2, l1)((acc, head) => Cons(head, acc)) // given List(1, 2) and List(3, 4) => foldLeft(List(2), Cons(1, List(3, 4))) => foldLeft(Nil, Cons(2, Cons(1, List(3, 4)))) => List(2, 1, 3, 4)
  }

  def appendWithFoldRight[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)((head, acc) => Cons(head, acc))
  }


  // TODO: 3.15 Hard
  // 3.16
  // 3.17
  // 3.18
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    list match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  // TODO: 3.19
  // TODO: 3.20
  // TODO: 3.21
  // 3.22
  // 3.23
  // TODO: 3.24 Hard
  // TODO: 3.25
  // TODO: 3.26
  // TODO: 3.27
  // TODO: 3.28
  // TODO: 3.29




}

object ListMain extends App {
  private val listTest: List[Int] = List(1, 2, 3, 4)
  private val listTest2: List[Int] = List(5, 6, 7, 8)


  //  println(dropWhile(listTest)((x: Int) => x > 5))
  //
  //  println(foldRight(listTest, 0)((x, y) => x + y))
  //
    println(foldRight(listTest, 1.0)((x, y) => x * y))

  // 3.8
  //  println(foldRight(listTest, Nil: List[Int])(Cons(_, _)))

  //  println(listTest)

  // 3.9
  println(length(listTest))

  // 3.10 foldLeft
  println(foldLeft(listTest, 1.0)((x, y) => x * y))

  // 3.11 Others functions with foldLeft
  println(foldLeft(listTest, 1.0)(_ * _))

  // 3.12 Reverse Order
  println(reverseOrderList(listTest, Nil))
  println(reverseOrderListWithFold(listTest))

  // 3.14
  println(appendWithFoldLeft(listTest, listTest2))
  println(appendWithFoldRight(listTest, listTest2))

  // 3.18
  println(map(listTest)(_+2))


}
