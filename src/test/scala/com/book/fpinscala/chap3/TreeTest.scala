package com.book.fpinscala.chap3

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class TreeTest extends AnyFunSuite {
  val leafTest = Leaf(1)
  val treeTest = Branch(Branch(Leaf(3), Leaf(1)), Leaf(8))
  val treeTest1 = Branch(Branch(Leaf(3), Leaf(1)), Branch(Leaf(8), Leaf(6)))
  val treeTest2 = Branch(Branch(Leaf(10), Leaf(235)), Branch(Leaf(8), Branch(Leaf(9), Leaf(1000))))
  val treeTest3 = Branch(Leaf(3), Leaf(1))
  val treeTest4 = Branch(Branch(Leaf(10), Leaf(235)), Branch(Leaf(8), Branch(Leaf(9), Branch(Leaf(8), Branch(Leaf(9), Leaf(1000))))))


  test("map for Leaf") {
    val treeResult = Tree.map(leafTest)(x => x + 2)
    treeResult shouldEqual Leaf(3)
  }
}



