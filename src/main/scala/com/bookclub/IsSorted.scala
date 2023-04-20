package com.bookclub

import scala.annotation.tailrec

object Caleb extends App {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(i: Int): Boolean = {
      if (i >= as.length) true
      else if (ordered(as(i), as(i - 1))) go(i + 1)
      else false
    }

    go(1)
  }

  val arr = (1 to 10).toArray
  val arr2 = Array(2, 3, 1)
  val arr3 = Array(5, 1, 2)
  val arr4 = Array(1, 1, 1, 2)

  Array(arr, arr2, arr3, arr4).map(arr => {
    println(arr.toList)
    println(isSorted(arr, (x: Int, y: Int) => y <= x))
    println()
  })
}

object Ch2_IsSorted {
  private def isSorted[A](arr: Array[A], isOrdered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean =
      if (i >= arr.length - 1) true
      else if (!isOrdered(arr(i + 1), arr(i))) false
      else loop(i + 1)

    loop(0)
  }

  private def isSorted_v2[A](arr: Array[A], isOrdered: (A, A) => Boolean): Boolean =
    arr.zipWithIndex.map {
      case (_, i) if !(i >= arr.length - 1) => isOrdered(arr(i + 1), arr(i))
      case _ => true
    }.forall(_ == true)

  def main(args: Array[String]): Unit = {
    /* V1 Assertions */
    val shouldBeSortedInts = isSorted[Int](List(1, 2, 3, 4).toArray, _ > _)
    assert(shouldBeSortedInts)
    val shouldBeSortedStringLengths = isSorted[String](List("A", "AB", "ABC").toArray, _.length > _.length)
    assert(shouldBeSortedStringLengths)

    val shouldNotBeSortedInts = isSorted[Int](List(2, 4, 3, 1).toArray, _ > _)
    assert(!shouldNotBeSortedInts)
    val shouldNotBeSortedStringLengths = isSorted[String](List("AB", "A", "ABC").toArray, _.length > _.length)
    assert(!shouldNotBeSortedStringLengths)

    /* V2 Assertions */
    val shouldBeSorted_v2 = isSorted_v2[Int](List(1, 2, 3, 4).toArray, _ > _)
    assert(shouldBeSorted_v2)
    val shouldNotBeSorted_v2 = isSorted_v2[Int](List(2, 4, 3, 1).toArray, _ > _)
    assert(!shouldNotBeSorted_v2)
  }
}

object Erick extends App {
  private def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(e: A, as: Array[A]): Boolean = {
      if (as.isEmpty) true
      else if (ordered(e, as.head)) go(as.head, as.tail)
      else false
    }

    go(as.head, as.tail)
  }

//  def main(args: Array[String]): Unit = {
//    print(isSorted(Array(5, 4, 3, 2, 1), (a: Int, b: Int) => a > b))
//  }
}













object IsSorted extends App {
  private def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  println(s"for ${Array(1, 2, 3).mkString("Array(", ", ", ")")} is ascending? " + isSorted(Array(1, 2, 3), (x: Int, y: Int) => x < y)) // isAscending?
  println(s"for ${Array(1, 2, 3).mkString("Array(", ", ", ")")} is descending? " + isSorted(Array(1, 2, 3), (x: Int, y: Int) => x > y)) // isDescending?
  println(s"for ${Array(1, 3, 2).mkString("Array(", ", ", ")")} is sorted? " + isSorted(Array(1, 3, 2), (x: Int, y: Int) => x > y)) // should return false
}

object IsSortedNonRecursive extends App {
  private def isSortedNonRecursive[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.zip(as.tail).forall { case (x, y) => ordered(x, y) }
  }

  println(Array(1, 2, 3).zip(Array(1, 2, 3).tail).mkString("Array(", ", ", ")"))

  println(s"for ${Array(3, 2, 1).mkString("Array(", ", ", ")")} is descending? " + isSortedNonRecursive(Array(3, 2, 1), (x: Int, y: Int) => x > y))
}

object IsSortedPatternMatch extends App {
  @tailrec
  private def isSortedRecursivePatternMatch[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as match {
      case Array() => true
      case Array(_) => true
      case _ => ordered(as.head, as.tail.head) && isSortedRecursivePatternMatch(as.tail, ordered)
    }
  }

  println(s"for ${Array(3, 2, 1).mkString("Array(", ", ", ")")} is descending? " + isSortedRecursivePatternMatch(Array(3, 2, 1), (x: Int, y: Int) => x > y))
}
