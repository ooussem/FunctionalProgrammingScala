package com.book.fpinscala.chap3

import com.book.fpinscala.chap3.List.{dropWhile, foldRight}

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


  def dropWhile[T](list: List[T])(f: T => Boolean): List[T] = {
    list match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => list
    }
  }


  def add[T](xs: List[T], x: T) = {
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

  def init[T](l: List[T]): List[T] = {
    l match {
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
   * Giving a list: Cons(2, 3, Nil), we want =>
   *    f(2, foldRight(3, 0)(f)) => 2 + f(3, foldRight(Nil, 0)(f)) => 2 + (3 + foldRight(Nil, 0)(f)) => 2 + 3 + 0 => 5
   *
   */
  def foldRight[A, B](list: List[A], defaultValue: B)(f: (A, B) => B): B = {
    list match {
      case Nil => defaultValue
      case Cons(x, xs) => f(x, foldRight(xs, defaultValue)(f))
    }
  }

}

object ListMain extends App {
  private val listTest: List[Int] = List(2, 3, 5, 6, 10, 5)

  println(dropWhile(listTest)((x: Int) => x > 5))

  println(foldRight(listTest, "")((x, y) => x + y))

}
