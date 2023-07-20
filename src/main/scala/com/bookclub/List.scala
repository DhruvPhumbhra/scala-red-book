package com.bookclub

import com.bookclub.List._

import scala.annotation.tailrec
//import com.bookclub.ListExercises._

sealed trait List[+A]

case object Nil extends List[Nothing] {
  final override def toString: String = "List()"
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {

  final override def toString: String = {
    def loop(tail: List[A]): String = {
      tail match {
        case Nil => ")"
        case Cons(h, Nil) => s", $h)"
        case Cons(h, t) => s", $h${loop(t)}"
      }
    }

    s"List($head${loop(tail)}"
  }

//  final override def toString: String = {
//    def commaDelimitedStringify(l: List[A]): String = {
//      l match {
//        case Cons(last, Nil) => last.toString
//        case Cons(head, tail) => head.toString + ", " + commaDelimitedStringify(tail)
//      }
//    }
//
//    @tailrec
//    def commaDelimitedStringifyTailRec(acc: String, l: List[A]): String = {
//      l match {
//        case Cons(last, Nil) => acc + last.toString // last element is special because it's not followed by a comma
//        case Cons(head, tail) => commaDelimitedStringifyTailRec(acc + head.toString + ", ", tail)
//      }
//    }
//
//    commaDelimitedStringifyTailRec("List(", Cons(head, tail)) + ")"
//    foldLeft(Cons(head, tail), "List(")((acc, x) => acc + x.toString + ", ").dropRight(2) + ")" // 1, 2, 3,
//  }
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def sumFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def productFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)((acc, head) => acc * head)

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

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => 1 + acc)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, head) => Cons(head, acc))
//    l match {
//    case Nil => soFar
//    case Cons(head, tail) => reverse(tail, Cons(head, soFar))
//  }

  // this doesn't work, don't use it
  def reverseUsingFoldRight[A](l: List[A], soFar: List[A] = Nil): List[A] =
    l match {
      case Nil => soFar
      case Cons(head, tail) => Cons(head, reverseUsingFoldRight(tail, soFar))
    }

  def foldRightUsingFoldLeft[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), acc)((b, a) => f(a, b))

  def foldLeftUsingFoldRight[A, B](as: List[A], acc: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(acc)

  def appendUsingFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))

  def appendUsingFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((b, a) => Cons(a,b))

  def concatLists[A](list: List[List[A]]): List[A] = foldRight(list, Nil: List[A])(appendUsingFoldRight)

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRightUsingFoldLeft(as, Nil: List[B])((x, y) => Cons(f(x), y))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concatLists(map(as)(f))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightUsingFoldLeft(as, Nil: List[A])((h, t) => if(f(h)) Cons(h, t) else t)

  def filterUsingFlatmap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) List(a) else Nil)

  def addListPairs(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
//    case (Cons(h1, t1), Nil) => Cons(h1, t1)
//    case (Nil, Cons(h2, t2)) => Cons(h2, t2)
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addListPairs(t1, t2))
    case (_, _) => Nil
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case (_, _) => Nil
  }

  // take the first n elements from a list
  def take[A](l: List[A], n: Int): List[A] = reverse(drop(reverse(l), length(l) - n))

}

object TestTake extends App {
  println(take(List(1, 2, 3, 4), 2)) // List(1, 2)
}

object ListSubsequence extends App {
  def hasSubsequence[A](sup: collection.immutable.List[A], sub: collection.immutable.List[A]): Boolean = {
    sup match {
      case _ :: tail => if (sup.take(sub.length) == sub) true else hasSubsequence (tail, sub)
      case collection.immutable.List() => false
    }
  }

  // true cases
  println(hasSubsequence(collection.immutable.List(1, 2, 3, 4), collection.immutable.List(1, 2, 3)))
  println(hasSubsequence(collection.immutable.List(1, 2, 3, 4), collection.immutable.List(2, 3)))
  println(hasSubsequence(collection.immutable.List(1, 2, 3, 4), collection.immutable.List(4)))
  println(hasSubsequence(collection.immutable.List(1, 2, 3, 4), collection.immutable.List()))


  // false cases
  println(hasSubsequence(collection.immutable.List(1, 2, 3, 4), collection.immutable.List(1, 4)))
  println(hasSubsequence(collection.immutable.List(1, 2, 3, 4), collection.immutable.List(4, 5)))
  println(hasSubsequence(collection.immutable.List(1, 2, 3, 4), collection.immutable.List(5)))
  println(hasSubsequence(collection.immutable.List(), collection.immutable.List(4)))
}

object AddListPairsTest extends App {
  println(addListPairs(List(1, 2, 3), List(4, 5, 6)))
  println(addListPairs(List(1, 2, 3), List(4, 5, 6, 7)))
  println(addListPairs(List(1, 2, 3), List(4, 5, 6, 7, 8)))
  println(zipWith(List(1, 2, 3, 4, 5), List(6, 7))(_ + _))
}

object FlatMapExercise extends App {

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))

  def concatenate[A](alist: List[List[A]]): List[A] = alist match {
    case Nil => Nil
    case Cons(head, tail) => append((head), concatenate(tail))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(head, tail) => append(f(head), flatMap(tail)(f))
  }

  println(flatMap(List(1,2,3))(i => List(i, i)))
  println(flatMap(List(1.1, 2.2, 3.3))(i => List(i, i * 2)))
  println(flatMap(List('a', 'b', 'c'))(i => List(i, i.toString + i)))

}




// Excercise 3.15: Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.

object ConcatenateListsExercise extends App {
  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))

  def concatenate[A](alist: List[List[A]]): List[A] =  alist match {
    case Nil => List()
    case Cons(head, tail) => append((head), concatenate(tail))
  }

  // James
  def concatLists[A](list: List[List[A]]): List[A] = foldRight(list, Nil: List[A])(append)

  println(concatenate(List(List(1,2,3), List('a','b','c'), List(1.1, 2.2, 3.3, 4.4))));


  flatMap(List(1,2,3))(i => List(i, i)) // List(1,1,2,2,3,3)
}















object AppendExercise extends App {
  def appendUsingFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))

  def appendUsingFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((b, a) => Cons(a,b))

  val listOne: List[Int] = List(1,2,3)
  val listTwo: List[Int] = List(4,5,6)
  val listA: List[Char] = List('a','b','c')

  println(appendUsingFoldRight(listOne,listTwo))
  println(appendUsingFoldLeft(listOne,listA))

}

object add1 extends App {
  def add1(alist: List[Int]): List[Int] =
    foldRight(alist, Nil: List[Int])((x, y) => Cons(x + 1, y))

  def doubleToString(alist: List[Double]): List[String] =
    foldRight(alist, Nil: List[String])((h, t) => Cons(h.toString, t))

  // Lauren
  def addOne(list: List[Int]): List[Int] =
    reverse(foldLeft(list, Nil: List[Int])((acc: List[Int], head: Int) => Cons(head + 1, acc)))


  val listEmpty: List[Int] = List()
  val listInt: List[Int] = List(1,2,3)

  println(listEmpty)
  println(add1(listEmpty))

  println(listInt)
  println(add1(listInt))
  println(addOne(listInt))

}

// Exercise 3.17: Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString to convert some d: Double to a String.
object doubleToString extends App {
  def doubleToString(alist: List[Double]): List[String] =
    foldRight(alist, Nil:List[String])((h, t) => Cons(h.toString, t))

  def doubleToStringUsingFoldLeft(alist: List[Double]): List[String] =
    reverse(foldLeft(alist, Nil:List[String])((t, h) => Cons(h.toString, t)))



  val listDouble: List[Double] = List(1.2, 2.3, 3.4)
  val stringList: List[String] = doubleToString(listDouble)

  println(doubleToString(listDouble))
  println(doubleToStringUsingFoldLeft(listDouble))
}

object MapAndFilterExercise extends App {
  val listInt: List[Int] = List(1,2,3)

  println(map(listInt)(_ + 1))
  println(map(listInt)(_.toString))

  println(filter(List(1, 2, 3, 4, 5, 8))(_ % 2 == 0))

}

object EvenMoreLists extends App {
//  println(foldRightUsingFoldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_,_)))
  println(reverseUsingFoldRight(List(1, 2, 3)))
  println(foldLeftUsingFoldRight(List(1, 2, 3), Nil: List[Int])((b, a) => Cons(a, b)))
}

object MoreLists extends App {
//  (1 + 2 + (3 + 0))
//  Cons(1, Cons(2, (Cons(3, Nil))))
  println(foldLeft(List(1,2,3), Nil:List[Int])((b, a) => Cons(a,b)))
  println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _)))

  println(foldRight(List(1, 2, 3), 0)(_ - _))
  println(foldLeft(List(1, 2, 3), 0)(_ - _))

//  val listInt = List(1, 2, 3, 4, 5);
//  println(foldLeft(listInt, 0)((x, y) => x + y));
}

object UsingLists extends App {

  val ex1: List[Double] = Nil
  val ex2: List[Int] = Cons(1, Nil)
  val ex3: List[String] = Cons("a", Cons("b", Nil))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(h, t) => h + sum(t)
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case _ => 101
  }

  val a: List[Int] = Cons(1, Cons(2, Cons(3, Nil))) // List(1, 2)

  println(a)

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
