package com.bookclub


// Erick's solution
object Fib extends App {
  private def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int = {
      if (n <= 1) acc1
      else go(n - 1, acc2, acc1 + acc2)
    }

    go(n, 0, 1)
  }

  (0 to 20).toList.map(fib).foreach(println)
}

// Cassidy's solution
object Fibonacci {
  @annotation.tailrec
  private def fibonacci(upperBound: Int, currentList: Array[Int] = Array()): Array[Int] = {
    if (currentList.length < upperBound) {
      fibonacci(
        upperBound,
        currentList :+ getNext(currentList)
      )
    } else {
      currentList
    }
  }

  private def getNext(currentList: Array[Int]): Int = {
    currentList.length match {
      case 0 => 0
      case 1 => 1
      case _ => currentList.last + currentList(currentList.length - 2)
    }
  }

  def main(args: Array[String]): Unit = {
    println(fibonacci(0).mkString("\n"))
  }
}

// Shreya's solution - doesn't work
object MyModuleFib {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(x: Int, n: Int, current: Int, previous: Int): Int = {
      if (n == 0) 0
      else if (n == 1 || n == 2) 1
      else if (x == n) current
      else go(x + 1, n , current + x, previous)
    }

    go(0,n, 1, 0)

  }

  def main(args: Array[String]): Unit =
    (1 to 20).toList.map(fib).foreach(println)
}

// Caleb's solution
object AnotherFib {
  def fib2(n: Int): Int = {
    def go(i: Int, x: Int, y: Int): Int = {
      if (i == n) x
      else go(i + 1, y, x + y)
    }

    go(1, 0, 1)
  }
}


object AllFibSolutions extends App {
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int, cur: Int): Int = n match {
      case 0 => acc
      case _ => loop(n - 1, cur, acc + cur)
    }

    loop(n, 0, 1)
  }

  def nonTailRecFib(n: Int): Int = {
    if (n <= 0) 0
    else if (n == 1) 1
    else nonTailRecFib(n - 2) + nonTailRecFib(n - 1) // f(n) = f(n-1) + f(n-2)
  }

  def iterativeFib(n: Int): Int = {
    var x = 0
    var y = 1
    var z = 1
    for (i <- 0 until n) {
      x = y // cur
      y = z // prev
      z = x + y // next
    }
    x
  }

  def absolutelyInsaneSolution(n: Int): Int = {
    var l = scala.collection.mutable.Buffer(0, 1)
    scala.collection.immutable.List.fill(n)(l.addOne(l.zip(l.tail).map { case (x, y) => x + y }.last)).flatten.dropRight(1).last
  }

  lazy val allSolutions: scala.collection.immutable.List[(Int => Int, String)] =
    scala.collection.immutable.List(
      fibonacci _ -> "fibonacci",
      nonTailRecFib _ -> "nonTailRecFib",
      iterativeFib _ -> "iterativeFib",
      absolutelyInsaneSolution _ -> "absolutelyInsaneSolution"
    )

  allSolutions.foreach { case (f, name) =>
    println(s"solution for $name")
    (1 until 20).toList.map(i => f(i)).foreach(println)
  }
}
