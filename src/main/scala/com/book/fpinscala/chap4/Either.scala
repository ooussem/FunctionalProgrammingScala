package com.book.fpinscala.chap4

sealed trait Either[+E, +A]{

  // exo 4.6
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(error) => Left(error)
      case Right(value) => Right(f(value))
    }
  }

  def flatMap[EE>:E, B](f: A => Either[EE,B]): Either[EE,B] = {
    this match {
      case Left(error) => Left(error)
      case Right(value) => f(value)
    }
  }

  def orElse[EE>:E, B>:A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(value) => Right(value)
    }
  }

  def map2[EE>:E, B, C](bEither: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(aValue => bEither.map(bValue => f(aValue, bValue)))

    for{ aValue <- this; bValue <- bEither } yield f(aValue, bValue)
  }

}

object EitherComp {

  // Add ex 4.7
  def sequence[E, A](listEither: List[Either[E, A]]): Either[E, List[A]] = {
    listEither match {
      case head :: tail => head.flatMap(hh => sequence(tail).map(tt => hh :: tt))
      case Nil => Right(Nil)
    }

    listEither match {
      case head :: tail => head.map2(sequence(tail))(_ :: _)
      case Nil => Right(Nil)
    }

    listEither.foldRight(Right(List[A]()): Either[E, List[A]])((e, l) => e.map2(l)(_ :: _))
  }

  def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    list match {
//      case head :: tail => f(head).flatMap(hh => traverse(tail)(f).map(tt => hh :: tt))
      case head :: tail => f(head).map2(traverse(tail)(f))(_ :: _)
      case Nil => Right(Nil)
    }

    list.foldRight(Right(List[B]()): Either[E, List[B]])((a, b) => f(a).map2(b)(_ :: _))
  }



}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
