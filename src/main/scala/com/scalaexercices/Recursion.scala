package com.scalaexercices

import scala.annotation.tailrec

object Recursion {

  def factorial(n: Int): Int = {
    if(n < 0 || n == 0) 1
    else n * factorial(n - 1)
  }

  @tailrec
  def factorialTailRec(x: Int, result: Int): Int = {
    if(x == 1) result
    else factorialTailRec(x - 1, x * result)
  }
}
