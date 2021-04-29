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
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
