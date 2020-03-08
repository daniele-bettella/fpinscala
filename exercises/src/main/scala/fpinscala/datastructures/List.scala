package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def sumfl(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def productfl(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Cons(h, Nil)
      case Cons(x, xs) => Cons(h, xs)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) if n <= 1 => xs
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case l@Cons(x, xs) => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new IllegalArgumentException("called init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def length[A](l: List[A]): Int = {
    def go[A](as: List[A], s: Int): Int = {
      as match {
        case Nil => s
        case Cons(_, xs) => go(xs, s + 1)
      }
    }

    go(l, 0)
  }

  def length2[A](l: List[A]): Int = {
    foldRight(l, 0)((_, b) => b + 1)
  }

  /**
   * foldRight(List(w, x, y), z)(f)
   * f(w, foldRight(List(x, y), z)(f))
   * f(w, f(x, foldRight(List(y), z)(f)))
   * f(w, f(x, f(y, foldRight(Nil, z)(f))))
   * f(w, f(x, f(y, z)))
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
   * foldLeft(List(w, x, y), z)(f)
   * foldLeft(List(x, y), f(z, w))(f)
   * foldLeft(List(y), f(f(z, w), x))(f)
   * foldLeft(Nil, f(f(f(z, w), x), y))(f)
   * f(f(f(z, w), x), y)
   */
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  /**
   * foldRightfl(List(w, x, y), z)(f: (A, B) => B)
   * foldLeft(List(y, x, w), z)((b, a) => f(a, b))
   * foldLeft(List(x, w), f(y, z))((b, a) => f(a, b))
   * foldLeft(List(w), f(x, f(y, z)))((b, a) => f(a, b))
   * foldLeft(Nil, f(w, f(x, f(y, z))))((b, a) => f(a, b))
   * f(w, f(x, f(y, z)))
   */
  def foldRightfl[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b, a) => f(a, b))
  }

  /**
   * foldLeftfr(List(w, x, y), z)(f: (B, A) => B)
   * foldRight(List(y, x, w), z)(f1: (a, b) => f(b, a))
   * f1(y, foldRight(List(x, w), z)(f1))
   * f1(y, f1(x, foldRight(List(w), z)(f1)))
   * f1(y, f1(x, f1(w, foldRight(Nil, z)(f1))))
   * f1(y, f1(x, f1(w, z)))
   * f1(y, f1(x, f(z, w)))
   * f1(y, f(f(z, w), x))
   * f(f(f(z, w), x), y)
   */
  def foldLeftfr[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(l), z)((a, b) => f(b, a))
  }

  def lengthfl[A](l: List[A]): Int = {
    foldLeft(l, 0)((b, _) => b + 1)
  }

  def appendfl[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(reverse(l1), l2)((acc, a) => Cons(a, acc))
  }

  def appendfr[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_, _))
  }

  def reverse[A](l: List[A]): List[A] = {
    def go[A](as: List[A], reversed: List[A]): List[A] =
      as match {
        case Nil => reversed
        case Cons(x, xs) => go(xs, Cons(x, reversed))
      }

    go(l, Nil)
  }

  def reverse2[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((xs, x) => Cons(x, xs))
  }

  def concatenate[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])((acc, a) => foldRight(acc, a)(Cons(_, _)))
  }

  def plus1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight(l, Nil: List[B])((a, acc) => append(f(a), acc))
  }

  def toStringList(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((a, acc) => Cons(s"$a", acc))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((a, acc) => Cons(f(a), acc))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)
  }

  def filterfm[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) Cons(a, Nil) else (Nil))
  }

  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(a + b, addPairwise(as, bs))
    }
  }

  def applyPairwise[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), applyPairwise(as, bs)(f))
    }
  }

  def hasSubsequence[A](l: List[A], s: List[A]): Boolean = {

    def go(currentL: List[A], currentS: List[A]): Boolean =
      currentL match {
        case Nil if currentS == Nil => true
        case Nil if currentS != Nil => false
        case Cons(x, xs) => currentS match {
          case Cons(y, ys) if (x == y) => // we have a match, keep checking this sequence
            go(xs, ys)
          case Cons(y, ys) => // no match, go back to start with remainder of list
            hasSubsequence(tail(l), s)
          case Nil => // we exhausted the sequence, we have a match
            true
        }
      }

    l match {
      case Nil if s == Nil => true
      case Nil => false
      case l => go(l, s)
    }
  }
}
