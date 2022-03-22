package com.book.fpinscala.chap5

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case Empty => None
  }


  // exo 5.1
  // stackoverflow with large list
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toListTailRec: List[A] = {
    @tailrec
    def go(stream: Stream[A], accList: List[A]): List[A] = {
      stream match {
        case Cons(h, t) => go(t(), h() :: accList)
        case _ => accList
      }
    }

    go(this, List()).reverse
  }

  def toListTailRecFast: List[A] = {
    val buf = new ListBuffer[A]
    @tailrec
    def go(stream: Stream[A]): List[A] = {
      stream match {
        case Cons(h, t) => {
          buf += h()
          go(t())
        }
        case _ => buf.toList
      }
    }

    go(this)
  }

  // exo 5.2
  def take(n: Int): Stream[A] = {
    def go(stream: Stream[A], acc: Int): Stream[A] = {
      stream match {
        case Cons(h, t) if acc > 0 => cons(h(), t().take(n - 1))
        case _ => Stream.empty
      }
    }
    go(this, n)
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // exo 5.3
  def takeWhile(p: A => Boolean): Stream[A] = {
    @tailrec
    def go(stream: Stream[A], streamAcc: Stream[A]): Stream[A] = {
      stream match {
        case Cons(h, t) if p(h()) =>  go(t(), cons(h(), streamAcc))
        case _ => streamAcc
      }
    }
    go(this, empty[A])
  }


  def takeWhileBookSolution(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t().takeWhileBookSolution(f))
    case _ => empty
  }


  def foldRight[B](default: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(default)(f))
    case _ => default
  }

  def exist[B](p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  // exo 5.5 def takeWhile with foldRight()
  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else empty)
  }

  // exo 5.4
  def forAll(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) && b)
  }

  // exo 5.6
  def headOptionWithFoldRight: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  // exo 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => cons(f(a), b)) // equivalent à f = cons(g(h()), t().foldRight(emptyStream[B])(f)

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def appendElement[B >: A](x: => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => cons(x, cons(a, b)))

  def append[B >: A](otherStream: => Stream[B]): Stream[B] =
    foldRight(otherStream)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = {
    filter(p).headOption
  }

  // exo 5.11
  // corecursive function => producer of data whereas recursive function is a consumer of data
  def unfold[T, S](s: S)(f: S => Option[(T, S)]): Stream[T] = {
    f(s) match {
      case Some(value) => cons(value._1, unfold(value._2)(f))
      case None => Stream.empty
    }
  }

  def mapWithUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  // This solutions is correct but not efficient because when i==n we call again the tail (and we needn't it) and we increment again (computation useless)
  def takeWithUnfold(n: Int): Stream[A] = {
    unfold((this, 1)) { // My State is a tuple of Stream and number
      case (Cons(h, t), i) if i <= n => Some((h(), (t(), i+1)))
      case _ => None
    }
  }

  def takeWithUnfoldMoreEfficient(n: Int): Stream[A] = {
    unfold((this, 1)) {
      case (Cons(h, _), i) if i == n => Some((h(), (empty, n+1))) // we process again n + 1 :(
      case (Cons(h, t), i) if i < n => Some((h(), (t(), i+1)))
      case _ => None
    }
  }

  def takeViaUnfoldSolutionBook(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0))) // => more efficient because we just evaluate what we want and no need of incrementation
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  // TODO
  def zipWith() = {

  }

  // TODO: others

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

  // exo 5.8
  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def constantEfficient[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // exo 5.9
  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  // TODO: Try again
  def fibsNotWork(stream: Stream[Int], beforeLast: Int = 0, acc: Int = 1): Stream[Int] = {
    stream match {
      case Empty => fibsNotWork(stream)
      case Cons(h, t) => stream.append(fibsNotWork(t(), h(), h() + acc))
    }
  }

  def fibsSolution: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, b + a))
    }
    go(0, 1)
  }

  // exo 5.11
  // corecursive function => producer of data whereas recursive function is a consumer of data
  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(s) match {
      case Some(value) => cons(value._1, unfold(value._2)(f))
      case None => Stream.empty
    }
  }

  // exo 5.12
  def fibsWithUnfold: Stream[Int] = {
    unfold((0, 1))(state => Some((state._1, (state._2, state._1 + state._2))))
  }

  def fromWithUnfold(n: Int): Stream[Int] = {
    unfold(n)(state => Some((state, state + 1)))
  }

  def constantWithUnfold[A](constant: A): Stream[A] = {
    unfold(constant)(_ => Some((constant, constant)))
  }

  def oneWithUnfold: Stream[Int] = {
   constantWithUnfold(1)
  }

}

object main extends App {
  // Lazy in parameter hasn't the same behavior of the lazy val
  // affect the lazy parameter in the lazy declaration will be delay the evaluation and cache the value
  def twoCall(b: Boolean, i: => Int): Int = {
    println("twoCall test")
    if (b) i + i else 0
  }
  println(s"twoCall = ${twoCall(true, { println("Hi!!"); 1 + 41} )}")

  def onceCall(b: Boolean, i: => Int): Int = {
    println("onceCall test")
    lazy val j = {println("FLAG1") ; i}
    println("FLAG2")
    if (b) j + j else 0
  }
  println(s"onceCall = ${onceCall(true, { println("Hi!!"); 1 + 41 } )}")

  def onceWoLazy(b: Boolean, i: Int): Int = {
    println("onceWoLazy test")
    if (b) i + i else 0
  }
  println(s"maybeTwice = ${onceWoLazy(true, { println("Hi!!"); 1 + 41} )}")


}
