package com.book.fpinscala.chap4

import scala.collection.immutable.List


sealed trait Option[+A] {

  // exo 4.1
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(value) => Some(f(value))
      case None => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(value) => value
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(value) => f(value)
      case None => None
    }
  }

  def flatMap_2[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case Some(_) => this
      case None => ob
    }
  }

  def orElse_2[B >: A](ob: => Option[B]): Option[B] = {
    this map (v => Some(v)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(value) if f(value) => Some(value)
      case _ => None
    }
  }

  def filter_2(f: A => Boolean): Option[A] = {
    this flatMap (v => if (f(v)) Some(v) else None)
  }

  // exo 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(seq: Seq[Double]): Option[Double] = {
      seq match {
        case xs => Some(xs.sum / xs.size)
        case Nil => None
      }
    }

    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = o => o.map(f)

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  // Personal example
  def salaryNetByMonth(brutYear: Double): Double = (brutYear / 12) * 0.75

  val salaryNetByMonthOpt: Option[Double] => Option[Double] = o => lift(salaryNetByMonth)(o)

  // lift with 2 parameters
  def salaryNetByMonth(brutYear: Double, monthNumber: Double): Double = (brutYear / monthNumber) * 0.75

  def map2[A, B, C](optA: Option[A], optB: Option[B])(f: (A, B) => C): Option[C] = {
    optA.flatMap(a => optB.map(b => f(a, b)))
  }

  val salaryNetByMonthOpt2: (Option[Double], Option[Double]) => Option[Double] = (oA, oB) => map2(oA, oB)(salaryNetByMonth)


  def map3[A, B, C, O](optA: Option[A], optB: Option[B], optC: Option[C])(f: (A, B, C) => O): Option[O] = {
    optA.flatMap(a => optB flatMap (b => optC.map(c => f(a, b, c))))
  }

  def map4[A, B, C, D, O](optA: Option[A], optB: Option[B], optC: Option[C], optD: Option[D])(f: (A, B, C, D) => O): Option[O] = {
    optA.flatMap(a => optB flatMap (b => optC.flatMap(c => optD.map(d => f(a, b, c, d)))))
  }

  // exo 4.4
  def sequenceWithFold[A](listOpt: List[Option[A]]): Option[List[A]] = {
    listOpt.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  }


}

object Option {

  object WithChap3List {

    import com.book.fpinscala.chap3.{Cons, List, Nil}
    // exo 4.4
    def sequence[A](listOpt: List[Option[A]]): Option[List[A]] = {
      listOpt match {
        case Cons(head, tail) => head flatMap (opt => sequence(tail) map (t => Cons(opt, t)))
        case Nil => Some(Nil)
      }
    }

    def sequenceWithFoldRight[A](listOpt: List[Option[A]]): Option[List[A]] = {
      def map2[A, B, C](optA: Option[A], optB: Option[B])(f: (A, B) => C): Option[C] = {
        optA.flatMap(a => optB.map(b => f(a, b)))
      }

      List.foldRight(listOpt, Some(Nil): Option[List[A]])((optA, list) => optA.flatMap(a => list.map(l => Cons(a, l))))
      List.foldRight(listOpt, Some(Nil): Option[List[A]])((optA, list) => map2(optA, list)(Cons(_, _)))
    }
  }


  object WithScalaList {

    import scala.collection.immutable.List

    def map2[A, B, C](optA: Option[A], optB: Option[B])(f: (A, B) => C): Option[C] = {
      optA.flatMap(a => optB.map(b => f(a, b)))
    }

    // exo 4.4
    def sequence[A](listOpt: List[Option[A]]): Option[List[A]] = {
      listOpt match {
        case head :: tail => head flatMap (opt => sequence(tail) map (t => opt :: t))
        case Nil => Some(Nil)
      }
    }

    def sequenceWithFold[A](listOpt: List[Option[A]]): Option[List[A]] = {
      listOpt.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
    }
  }

}


case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]
