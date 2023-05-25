package com.bookclub

import com.bookclub.List.{foldRight, sum}

import scala.annotation.tailrec
//import com.bookclub.ListExercises._

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // (as: Seq[A]): List[A]
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new RuntimeException("oops")
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](l: List[A], a: A): List[A] = {
    l match {
      case Nil => throw new RuntimeException("oops")
      case Cons(_, tail) => Cons(a, tail)
    }
  }

  // List(1, 2, 3, 4) drop(2)(list ...) => List(3, 4)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
//    if (n <= 0) l
//    else
      l match {
      case _ if n <= 0 => l
      case Nil => Nil
      case Cons(_, tail) =>  drop(tail, n - 1)
    }
//  }
}

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _ => l
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil | Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
    case Nil => acc
    case Cons(head, tail) => f(head, foldRight(tail, acc)(f))
  }
}

//class MyList[A] extends List[A] {
//  def foo(x: A): A = ???
//}

object UsingLists extends App {

  val ex1: List[Double] = Nil
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))

//  val y = new MyList[Double]
//  y.foo(5.0)

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(h, t) => h + sum(t)
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case _ => 101
  }

  val a: List[Int] = Cons(1, Cons(2, Nil)) // List(1, 2)

  println(x)

//  List(1, 2, 3) match { case _ => 42 }
//  Nil match { case Cons(h,_) => h }
//  List(1, 2, 3) match { case Cons(_, t) => t }
//  List(1, 2, 3) match { case Nil => 42 }
}


object TestStuff extends App {
//  val xs: List[Int] = List(1, 2, 3, 4, 5)
//  val ex1: (Int => Boolean) => List[Int] = dropWhile(xs)
  //(x => x < 4)
//  println(ex1(x => x < 4))
//  println(ex1)

//  println(foldRight(List(1, 2, 3), 0)((x,y) => x + y))
  println(foldRight(List(1, 3, 2), 0)(_ + _))

}

// Cassidy's solution
object PatternMatching {
  private def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => list
      case Cons(_, t) => t
    }
  }

  def main(args: Array[String]): Unit = {
    println(tail(Nil))
    println(tail(List(1,2,3,4)))
  }
}
