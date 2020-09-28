package com.book.fpinscala.chap3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
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

  def drop[T](numToDrop: Int, list: List[T]): List[T] = {
    def go(i: Int, newList: List[T]): List[T] = {
      if(i == numToDrop) newList
      else {
        newList match {
          case Cons(_, t) => go(i + 1, t)
        }
      }
    }
    go(1, list)
  }


  def drop2[T](n: Int, list: List[T]): List[T] = {
    if(n <= 0) list
    else{
      list match {
        case Nil => Nil
        case Cons(_, t) => drop2(n - 1, t)
      }
    }
  }


  def dropWhile[T](f: T => Boolean, list: List[T]): List[T] = {
      list match {
        case Cons(h, t) =>
          if (f(h)) dropWhile(f, t)
          else t
        case _ => list
      }
  }

}
