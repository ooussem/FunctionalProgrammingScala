package com.book.fpinscala.chap3

import scala.annotation.tailrec

trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // TODO
//  def apply[A](xs: A*): Tree[Any] = {
//    if(xs.isEmpty) Tree[Nothing]
//    else {
//      xs match {
//        case Leaf(v) => Leaf(v)
//        case Branch(l, r) => apply()
//      }
//    }
//  }

  def size[A](tree: Tree[A]): Int = {
    @tailrec
    def go(tree: Tree[A], init: Int) : Int= {
      tree match {
        case Branch(l, _) => go(l, init + 1)
        case Branch(_, r) => go(r, init + 1)
        case Branch(l, r) => go(r, init + 1)
        case Leaf(_) => init + 1
      }
    }
    go(tree, 1)
  }
}

object TreeMain extends App {
  val treeTest = Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))
  println(Tree.size(treeTest))
}
