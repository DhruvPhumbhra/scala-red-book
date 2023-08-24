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

def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this.map(a => Some(a)).getOrElse(ob)
//  this match {
//    case None => ob
//    case Some(a) => Some(a)
//  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this.map(f).getOrElse(None)
//    this match {
//    case None => None
//    case Some(value) => f(value)
//  }

  def filter(f: A => Boolean): MyOption[A] = this.flatMap(value => if(f(value)) this else None)

  def fold[B](ifEmpty: => B)(f: A => B): B = this.map(f).getOrElse(ifEmpty)
}

case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object TestFoldOption extends App {
  println(Some(5).fold(0)(_ + 1))
  println((None: MyOption[Int]).fold(0)(_ + 1))
}

object TestEmployees extends App {
  case class Employee(name: String, department: Option[String] = Option.empty)

  Employee("name", Option.empty)

  val listOfEmployees = Seq(Employee("Joe", Option("dept")), Employee("otherName"))
  val listOfEmployees2 = Seq(Employee("Shreya", Option("devrel")), Employee("otherName"))
  def lookupByName(name: String): Option[Employee] = listOfEmployees.find(_.name == name)

  val joeDepartment: Option[String] = lookupByName("Joe").flatMap(_.department)
  val ShreyaDepartment: Option[String] =
    lookupByName("Shreya")
      .flatMap(_.department)
      .getOrElse("default dept")

  println(joeDepartment)
  println(ShreyaDepartment)
}

object TestApp {
  def main(args: Array[String]): Unit = {
    val optMap: MyOption[Int] = Some(2)
    assert(optMap.map(_ * 2) == Some(4))

    val optFlatMap: MyOption[Int] = Some(2)
    assert(optFlatMap.flatMap(a => Some(a * 2)) == Some(4))

    val optGetOrElse_Some: MyOption[Int] = Some(2)
    assert(optGetOrElse_Some.getOrElse(0) == 2)

    val optGetOrElse_None: MyOption[Nothing] = None
    assert(optGetOrElse_None.getOrElse(0) == 0)

    val optOrElse_Some: MyOption[Int] = Some(2)
    assert(optOrElse_Some.orElse(Some(4)) == Some(2))

    val optOrElse_None: MyOption[Int] = None
    assert(optOrElse_None.orElse(Some(4)) == Some(4))

    val optFilter: MyOption[Int] = Some(2)
    assert(optFilter.filter(_ == 2) == Some(2))
    assert(optFilter.filter(_ != 2) == None)
  }
}

object TestOptionFunctionsFlatMap extends App {
  val opt1: MyOption[Int] = Some(5)
  val opt2: MyOption[Int] = Some(10)
  val opt3: MyOption[Int] = None

  // Test with a function that returns Some
  val result1 = opt1.flatMap(num => Some(num * 2))
  println(result1) // Expected: Some(10)

  // Test with a function that returns None
  val result2 = opt1.flatMap(_ => None)
  println(result2) // Expected: None

  // Test with a function that transforms to another option
  val result3 = opt1.flatMap(num => if (num % 2 == 0) Some(num) else None)
  println(result3) // Expected: None

  // Test with an empty option
  val result4 = opt3.flatMap(num => Some(num * 2))
  println(result4) // Expected: None

  // Test with chaining flatMap calls
  val result5 = opt2.flatMap(num => Some(num + 5)).flatMap(num => Some(num * 3))
  println(result5) // Expected: Some(45)
}

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

  def mean_2(xs: IndexedSeq[Double]): MyOption[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
}



