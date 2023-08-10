package com.bookclub

import com.bookclub.Tree._

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def treeDepth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (treeDepth(left) max treeDepth(right))
  }

  def mapTree[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(mapTree(left)(f), mapTree(right)(f))
  }

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (size(left) + size(right)) // (Int, Int) => Int
  }

  def maxTree(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maxTree(left) max maxTree(right)
  }

  def myFold[A, B](tree: Tree[A])(acc: A => B)(f: (B, B) => B): B  = tree match {
    case Leaf(value) => acc(value)
    case Branch(left, right) => f(myFold(left)(acc)(f), myFold(right)(acc)(f))
  }

  def sizeUsingFold[A](tree: Tree[A]): Int =
    myFold(tree)(_ => 1)((l, r) => 1 + l + r)

  def treeDepthUsingFold[A](tree: Tree[A]): Int = myFold(tree)(_ => 0)((l, r) => 1 + (l max r))

  def maxUsingFold(tree: Tree[Int]): Int = myFold(tree)(value => value)(_ max _)


  /* List implementation of map
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRightUsingFoldLeft(as, Nil: List[B])((x, y) => Cons(f(x), y))
   */
  def mapUsingFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    myFold(tree)(a => Leaf(f(a)): Tree[B])((x, y) => Branch(x, y))

}

object TestFold extends App {
  val tree: Tree[Int] = Branch(
    Branch(Branch(Branch(Branch(Leaf(1), Leaf(14)), Leaf(14)), Leaf(14)), Leaf(14)),
    Branch(Leaf(256), Leaf(12))
  )

//  println(size(tree))
//  println(sizeUsingFold(tree))
//
//  println(treeDepth(tree))
//  println(treeDepthUsingFold(tree))
//
//  println(maxTree(tree))
//  println(maxUsingFold(tree))

  println(mapTree(tree)(_ + 1))
  println(mapUsingFold(tree)(_ + 1))
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