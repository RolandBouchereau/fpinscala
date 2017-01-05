package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => depth(left).max(depth(right)) + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size2[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(_ + _ + 1)

  def maximum2(tree: Tree[Int]): Int =
    fold(tree)(a => a)(_.max(_))

  def depth2[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)(_.max(_) + 1)

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    def fa(a: A): Tree[B] = Leaf(f(a))
    def fb(b1: Tree[B], b2: Tree[B]): Tree[B] = Branch(b1, b2)
    fold(tree)(fa)(fb)
  }

}
