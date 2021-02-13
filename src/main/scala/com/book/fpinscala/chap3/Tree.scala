package com.book.fpinscala.chap3

trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree {
  // TODO: apply function


  // exo 3.25
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Branch(left, right) => 1 + size(left) + size(right)
      case Leaf(_) => 1
    }
  }

  // exo 3.26
  def maxInTree(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(numb) => numb
      case Branch(left, right) => Integer.max(maxInTree(left), maxInTree(right))
    }
  }

  // exo 3.27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Branch(l, r) => 1 + Integer.max(depth(l), depth(r))
      case _ => 0
    }
  }


  // exo 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  // TODO: 3.29
}

object TreeMain extends App {

  val leafTest = Leaf(1)
  val treeTest = Branch(Branch(Leaf(3), Leaf(1)), Leaf(8))
  val treeTest1 = Branch(Branch(Leaf(3), Leaf(1)), Branch(Leaf(8), Leaf(6)))
  val treeTest2 = Branch(Branch(Leaf(10), Leaf(235)), Branch(Leaf(8), Branch(Leaf(9), Leaf(1000))))
  val treeTest3 = Branch(Leaf(3), Leaf(1))
  val treeTest4 = Branch(Branch(Leaf(10), Leaf(235)), Branch(Leaf(8), Branch(Leaf(9), Branch(Leaf(8), Branch(Leaf(9), Leaf(1000))))))

  println(Tree.size(treeTest1))

  println(Tree.maxInTree(treeTest2))


  println(Tree.depth(leafTest)) // 1
  println(Tree.depth(treeTest3)) // 2
  println(Tree.depth(treeTest)) // 3
  println(Tree.depth(treeTest1)) // 3
  println(Tree.depth(treeTest2)) // 4
  println(Tree.depth(treeTest4)) // 6

}
