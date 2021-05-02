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

  def mapMultiLeft[B, EE>:E](f: A => B, g: Seq[E] => Seq[EE]): Either[EE, B] = {
    this match {
      case MultiLeft(error) => MultiLeft(g(error))
      case Right(value) => Right(f(value))
    }
  }

  def flatMapMultiLeft[EE>:E, B](f: A => Either[EE,B])(g: Seq[E] => Either[EE,B]): Either[EE,B] = {
    this match {
      case MultiLeft(error) => g(error)
      case Right(value) => f(value)
    }
  }

  def map2MultiLeft[EE>:E, B, C](bEither: Either[EE, B])(f: (A, B) => C)(default: => C): Either[EE, C] = {
    flatMapMultiLeft(aValue => bEither.mapMultiLeft(bValue => f(aValue, bValue), seq => seq))(seqFirst => bEither.mapMultiLeft(_ => default, seqValue => seqFirst ++ seqValue ))
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

  // Add ex 4.8
  trait Return
  case class Person(name: Name, age: Age) extends Return
  case class Name(name: String) extends Return
  case class Age(age: Int) extends Return

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] = {
    mkName(name).map2(mkAge(age))(Person(_, _))
  }

  // Refactor
  def mkNameWithMultiLeft(name: String): Either[String, Name] =
    if (name == "" || name == null) MultiLeft(Seq("Name is empty."))
    else Right(Name(name))

  def mkAgeWithMultiLeft(age: Int): Either[String, Age] =
    if (age < 0) MultiLeft(Seq("Age is out of range."))
    else Right(Age(age))

  def mkPersonWithMultiLeft(name: String, age: Int)(default: Person): Either[String, Person] = {
    mkNameWithMultiLeft(name).map2MultiLeft(mkAgeWithMultiLeft(age))(Person(_, _))(default)
  }


}

case class Left[+E](value: E) extends Either[E, Nothing]
case class MultiLeft[+E](value: Seq[E]) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
