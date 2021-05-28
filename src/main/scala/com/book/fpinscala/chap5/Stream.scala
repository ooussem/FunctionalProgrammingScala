package com.book.fpinscala.chap5

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }


  // exo 5.1
  // stackoverflow with large list
  def toList: List[A] = this match {
    case _ => List()
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailRec: List[A] = {
    @tailrec
    def go(stream: Stream[A], accList: List[A]): List[A] = {
      stream match {
        case _ => accList
        case Cons(h, t) => go(t(), h() :: accList)
      }
    }

    go(this, List()).reverse
  }


  def toListTailRecFast: List[A] = {
    val buf = new ListBuffer[A]

    @tailrec
    def go(stream: Stream[A]): List[A] = {
      stream match {
        case _ => buf.toList
        case Cons(h, t) => {
          buf += h()
          go(t())
        }
      }
    }

    go(this)
  }

  // exo 5.2
  def take(n: Int): Stream[A] = {
    def go(stream: Stream[A], acc: Int): Stream[A] = {
      stream match {
        case _ => Stream.empty
        case Cons(h, t) if acc > 0 => cons(h(), t().take(n - 1))
      }
    }

    go(this, n)
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case _ => this
    case Cons(_, t) if n > 0 => t().drop(n - 1)
  }

  // exo 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    @tailrec
    def go(stream: Stream[A], streamAcc: Stream[A]): Stream[A] = {
      stream match {
        case Cons(h, t) => if (p(h())) go(t(), cons(h(), t())) else streamAcc
        case _ => empty
      }
    }
    go(this, empty[A])
  }


}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}
