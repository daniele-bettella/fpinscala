package fpinscala.datastrucutres

import fpinscala.datastructures._
import org.scalatest.flatspec.AnyFlatSpec

class ListSpec extends AnyFlatSpec {
  import List._
  val l = List(1, 2, 3, 4)
  val l2 = List(5, 6, 7, 8)

  "calling init on a list" should "return a new list with the last element removed" in {
    assert(init(l) == Cons(1, Cons(2, Cons(3, Nil))))
  }

  "calling a foldRight on a list with Nil and Cons as parameters" should "return a new copy of the list" in {
    assert(foldRight(l, Nil: List[Int])(Cons(_,_)) == l)
  }

  "calling length on a list" should "return the number of its elements" in {
    assert(length(l) == 4)
    assert(length2(l) == 4)
    assert(lengthfl(l) == 4)
  }

  "calling foldLeft on a list with zero and the sum function" should "return the sum of the numbers in the list" in {
    assert(foldLeft(l, 0)(_ + _) == 10)
  }

  "calling reverse on a list" should "return a new list with the items in reverse order" in {
    assert(reverse(l) == Cons(4, Cons(3, Cons(2, Cons(1, Nil)))) )
    assert(reverse2(l) == Cons(4, Cons(3, Cons(2, Cons(1, Nil)))) )
  }

  "append using foldLeft" should "still append the second list onto the first" in {
    assert(appendfl(l, l2) == append(l, l2))
  }

  "append using foldRight" should "still append the second list onto the first" in {
    assert(appendfr(l, l2) == append(l, l2))
  }

  "concatenate" should "create a single list from all the passed lists" in {
    val l3 = List(9, 10)
    val l4 = List(l, l2, l3)
    assert(concatenate(l4) == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  "plus1" should "return a new list with all elements with a 1 added" in {
    assert(plus1(l) != l)
    assert(plus1(l) == List(2, 3, 4, 5))
  }

  "toStringList" should "transform a list of doubles in a list o string that represents those doubles" in {
    val lDouble = List(1.0, 2.0, 3.0, 4.0)
    val lDoubleString = List("1.0", "2.0", "3.0", "4.0")
    assert(toStringList(lDouble) == lDoubleString)
  }

  "filtering only even numbers" should "return a list with only even numbers" in {
    assert(filter(l)(_ % 2 == 0) == List(2, 4))
    assert(filterfm(l)(_ % 2 == 0) == List(2, 4))
  }

  "flatMap" should "return a new list by concatenating the results of apply the passed function to the elements" in {
    assert(flatMap(l)(i => List(i, i)) == List(1, 1, 2, 2, 3, 3, 4, 4))
  }

  "hasSubsequence" should "return true if called on any subsequence" in {
    assert(hasSubsequence(l, List(1, 2)))
    assert(hasSubsequence(l, List(2, 3)))
    assert(hasSubsequence(l, List(2, 3, 4)))
    assert(hasSubsequence(l, List(4)))
  }

  "hasSubsequence" should "return false if called on things that are not subsequences" in {
    assert(hasSubsequence(l, List(2, 1)) == false)
    assert(hasSubsequence(l, List(2, 2)) == false)
    assert(hasSubsequence(l, List(1, 3, 4)) == false)
    assert(hasSubsequence(l, List(1, 3)) == false)
    assert(hasSubsequence(l, List(1, 5)) == false)
  }
}
