package fpinscala.datastrucutres

import fpinscala.datastructures._
import org.scalatest.flatspec.AnyFlatSpec

class TreeSpec extends AnyFlatSpec {

  import Tree._

  val t = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(5), Leaf(4)))

  "size" should "return the number of nodes in a Tree" in {
    assert(size(t) == 9)
    assert(sizef(t) == 9)
  }

  "maximum" should "return the maximum element in a tree of ints" in {
    assert(maximum(t) == 5)
    assert(maximumf(t) == 5)
  }

  "depth" should "return the maximum path length from root to leaf in the tree" in {
    assert(depth(t) == 4)
    assert(depthf(t) == 4)
  }

  "applying map on a tree of int with a function that adds 1 and transforms in string" should
    "return the starting tree with all elements incremented by 1 and transformed in strings" in {
    val t2 = Branch(Branch(Leaf("2"), Branch(Leaf("3"), Leaf("4"))), Branch(Leaf("6"), Leaf("5")))
    assert(map(t)(v => s"${v + 1}") == t2)
    assert(mapf(t)(v => s"${v + 1}") == t2)
  }

}
