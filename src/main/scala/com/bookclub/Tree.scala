package com.bookclub

import com.bookclub.Tree.mapTree

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maxTree(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maxTree(left) max maxTree(right)
  }

  def treeDepth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (treeDepth(left) max treeDepth(right))
  }

  def mapTree[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(mapTree(left)(f), mapTree(right)(f))
  }
}

object MaxTreeTest extends App {

  val tree: Tree[Int] = Branch(
    Branch(Branch(Branch(Branch(Leaf(1), Leaf(14)), Leaf(14)), Leaf(14)), Leaf(14)),
    Branch(Leaf(256), Leaf(12))
  )
//  println(maxTree(tree))
//  println(treeDepth(tree))

  println(mapTree(tree)(_ + 1))
}

object SizeTest {
  def main(args: Array[String]): Unit = {
    val tree: Tree[String] = Branch(
      Branch(Leaf("a"), Leaf("b")),
      Branch(Leaf("c"), Leaf("d"))
    )

    val size = Tree.size(tree)
    assert(size == 7)
    println(s"Size: $size")
  }
}