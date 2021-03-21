package com.book.fpinscala.chap4

sealed trait Option[+A] {
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
    this map(v => Some(v)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(value) if f(value) => Some(value)
      case _ => None
    }
  }

  def filter_2(f: A => Boolean): Option[A] = {
    this flatMap(v => if(f(v)) Some(v) else None)
  }

}
case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]
