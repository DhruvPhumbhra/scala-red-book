package com.bookclub

trait MyOption[+A]
case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]




object TestRT extends App {

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")

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

  println(failingFn2(12))
}

object AltsToExceptions extends App {
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length
}



