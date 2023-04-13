package com.bookclub

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc else go(n - 1, n * acc)

    go(n, 1)
  }

  private def formatAbs(x: Int): String = {
    s"The absolute value of $x is ${abs(x)}"
  }

  private def formatFactorial(n: Int): String = {
    s"The absolute value of $n is ${factorial(n)}"
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}

object HigherOrderFunctions extends App {

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc else go(n - 1, n * acc)
      // go(5, 5 * 1)
      // go(4, 4 * 5)
      // ... go(0, 120)
    }

    go(n, 1)
  }

  def factorialNotTailRecursive(n: Int): Int = {
    if (n <= 0) 1 else n * factorialNotTailRecursive(n - 1)

    // f(5) = > 5 * f(4) = [4 * f(3)] ... 5 * 4 * 3 * 2 * 1
  }

  println(factorial(5))
  println(factorialNotTailRecursive(5))
}
