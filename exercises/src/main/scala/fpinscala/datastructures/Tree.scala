package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](ts: Tree[A]): Int = {
    ts match {
      case Leaf(_) => 1
      case Branch(a, b) => 1 + size(a) + size(b)
    }
  }

  def maximum(ts: Tree[Int]): Int = {
    ts match {
      case Leaf(x) => x
      case Branch(a, b) => math.max(maximum(a), maximum(b))
    }
  }

  def depth[A](ts: Tree[A]): Int = {
    ts match {
      case Leaf(_) => 1
      case Branch(a, b) => 1 + math.max(depth(a), depth(b))
    }
  }

  def map[A, B](ts: Tree[A])(f: A => B): Tree[B] = {
    ts match {
      case Leaf(a) => Leaf(f(a))
      case Branch(a, b) => Branch(map(a)(f), map(b)(f))
    }
  }

  def fold[A, B](ts: Tree[A], l: A => B)(f: (B, B) => B): B = {
    ts match {
      case Leaf(a) => l(a)
      case Branch(a, b) => f(fold(a, l)(f), fold(b, l)(f))
    }
  }

  def sizef[A](ts: Tree[A]): Int = {
    fold(ts, (_: A) => 1)((b1: Int, b2: Int) => 1 + b1 + b2)
  }

  def maximumf(ts: Tree[Int]): Int = {
    fold(ts, (i: Int) => i)(math.max)
  }

  def depthf[A](ts: Tree[A]): Int = {
    fold(ts, (_: A) => 1)((b1: Int, b2: Int) => 1 + math.max(b1, b2))
  }

  def mapf[A, B](ts: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](ts, (a: A) => Leaf(f(a)))((b1, b2) => Branch(b1, b2))
  }


}