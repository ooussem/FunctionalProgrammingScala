package com.book.fpinscala.chap3

trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree {
  // TODO: apply function


  // exo 3.25
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(_) => 1
    }
  }

  // exo 3.26
  def maxInTree(tree: Tree[Int]): Int = {
    tree match {
      case Branch(l, r) => Integer.max(maxInTree(l), maxInTree(r))
      case Leaf(v) => v
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
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(v) => Leaf(f(v))
    }
  }

  // exo 3.29
  def fold[A, B](tree: Tree[A])(functionLeaf: A => B)(functionBranch: (B, B) => B): B = {
    tree match {
      case Leaf(v) => functionLeaf(v)
      case Branch(l, r) => functionBranch(fold(l)(functionLeaf)(functionBranch), fold(r)(functionLeaf)(functionBranch))
    }
  }

  def sizeWithFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)((leftSize, rightSize) => 1 + leftSize + rightSize) // fold(tree)(_ => 1)(1 + _ + _)
  }

  def maxInTreeWithFold(tree: Tree[Int]): Int = {
    fold(tree)(v => v)((leftMax, rightMax) => Integer.max(leftMax, rightMax))
  }

  def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(v => Leaf(f(v)).asInstanceOf[Tree[B]])((l, r) => Branch(l, r))
  }



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
