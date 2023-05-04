package com.bookclub

import scala.annotation.tailrec

object ListExercises {

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("list is empty") // or Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => throw new RuntimeException("list is empty") // or List(a) / Cons(a, Nil)
    case Cons(_, tail) => Cons(a, tail)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n <= 0 => l
    case Nil => Nil
    case Cons(_, tail) => drop(tail, n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x@Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

}
