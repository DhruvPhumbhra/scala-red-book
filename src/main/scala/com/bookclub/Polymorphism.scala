package com.bookclub


object Polymorphism extends App {
  private def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }
  private def findFirst(ss: Array[Int], p: Int => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  private def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

//  // monomorphic
//  println(findFirst(Array("foo", "bar", "baz"), "bar"))
//  println(findFirst(Array(1, 2, 3), 3))
//  println(findFirst(Array("foo", "bar", "baz"), "hello"))

  // polymorphic
  println(findFirst(Array("foo", "bar", "baz"), (x: String) => x == "bar"))
  println(findFirst(Array(1, 1, 2, 3, 5, 8), (x: Int) => x % 2 == 0))

  private case class Pair[A, B](first: A, second: B)
  println(findFirst(Array(Pair(1, 2L), Pair(3, 4L), Pair(6, 5L)), (x: Pair[Int, Long]) => x.first > x.second))
}

object Thing extends App {
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    b => f(a, b)
  }
}



object TypeDirectedProgramming extends App {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }
   // f(a, b, c) --> f(a)(b)(c)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}