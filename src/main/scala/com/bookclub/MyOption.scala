package com.bookclub

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = ???

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = ???

  def filter(f: A => Boolean): MyOption[A] = ???
}

case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object TestOptionFunctionsMap extends App {
  println(Some(5).map(_ + 1)) // this.map(_ + 1)
  println(Some(5).map(x => x + 1).map(_ + 2))
}

object TestOptionFunctions extends App {
//  Some(5).map(_ + 1)
//  println(Some(5).getOrElse(Await.result(Future.successful(Thread.sleep(10000)).map(_ => 4), Inf)))
  println(None.getOrElse(Await.result(Future.successful(Thread.sleep(10000)).map(_ => 4), Inf)))
//  println(None.getOrElse(Await.result(Future.successful(Thread.sleep(1000)).map(_ => 4), Inf)))
}

object TestRT extends App {

  def failingFn(i: Int): Int = {
    lazy val y: Int = throw new Exception("fail!")

    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    }
    catch {
      case e: Exception => 43
    }
  }

  println(failingFn(12))
//  println(failingFn2(12))
}

object AltsToExceptions extends App {
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  def mean_2(xs: IndexedSeq[Double]): MyOption[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}



