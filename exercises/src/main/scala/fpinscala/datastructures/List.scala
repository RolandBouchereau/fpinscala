package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val xl = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x // Not a match since 4 is third in the pattern, where 3 is expected.
    case Nil => 42 // Not a match since the List is not empty.
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // Match; first two values are 1 for x and 2 for y, which are added producing 3.
    case Cons(h, t) => h + sum(t) // Not checked against since a previous match succeeded.
    case _ => 101 // Not checked against since a previous match succeeded.
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil // Could throw an exception here. My choice to return Nil.
    // Might there be a way to enforce that l is non-empty on entry? With higher-kinded types?
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _) => Nil
    case (_, 0) => l
    case (Cons(h, t), _) => drop(t, n - 1)
  }

  //  @annotation.tailrec
  //  def drop[A](l: List[A], n: Int): List[A] = l match {
  //    case Nil => Nil
  //    case Cons(h, t) =>
  //      if (n == 0)
  //        l
  //      else
  //        drop(t, n - 1)
  //  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) =>
      dropWhile(t, f)
    case _ =>
      l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil | Cons(_, Nil) => Nil
    //    case Nil => Nil
    //    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /*
   Exercise 3.7
   Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it
   encounters a 0.0? Why or why not?

   No. The decision to halt recursion early would have to be performed in foldRight based on
   the value of a particular element in the list. No such provision resides in foldRight.
   Execution occurs over the entire list regardless of the value of any particular item.
  */

  /*
    Exercise 3.8
    See what happens when you pass Nil and Cons themselves to foldRight, like this:
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)). What do you think this
    says about the relationship between foldRight and the data constructors of List?

    This invocation of foldRight will just reproduce the list. This makes me think that
    at least some of the List data constructors use foldRight.
  */

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumUsingFoldLeft[A](l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productUsingFoldLeft[A](l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthUsingFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  /*
    Exercise 3.13
    Hard: Can you write foldLeft in terms of foldRight? How about the other way
    around? Implementing foldRight via foldLeft is useful because it lets us implement
    foldRight tail-recursively, which means it works even for large lists without overflowing
    the stack.

    To implement foldRight in terms of foldLeft, first reverse the input list then use a lambda
    which transposes the parameters and then calls the passed lambda. I don't think foldLeft can
    be implemented in terms of foldRight for the same reasons as the answer to exercise 3.10.
    I've considered a method where rather than passing the result of the function application,
    a by-name (lazy) wrapper function could be passed, but that exchanges heavy usage of the stack
    for heavy usage in the heap. I think that if performance is a desire, then a List is not likely
    the best structure to use.
  */

  def appendUsingFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def flatten[A](xss: List[List[A]]): List[A] = {
    val rl = reverse(xss)
    foldLeft(rl, Nil: List[A])((a, b) => (a, b) match {
      case (l, Nil) => l
      case (Nil, l) => l
      case (_, _) => append(b, a)
    })
  }

  def mapInts(xs: List[Int]): List[Int] =
    foldRight(xs, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  def mapDoubles(xs: List[Double]): List[String] =
    foldRight(xs, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((a, l) => append(f(a), l))

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addLists(as: List[Int], bs: List[Int]): List[Int] ={
    def loop(as: List[Int], bs: List[Int], sums: List[Int]): List[Int] = (as, bs) match {
      case (Cons(a, ta), Cons(b, tb)) => Cons(a + b, loop(ta, tb, sums))
      case _ => sums
    }
    loop(as, bs, Nil)
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] ={
    def loop(as: List[A], bs: List[B], sums: List[C]): List[C] = (as, bs) match {
      case (Cons(a, ta), Cons(b, tb)) => Cons(f(a, b), loop(ta, tb, sums))
      case _ => sums
    }
    loop(as, bs, Nil)
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def matchesSubsequence[B](sup: List[B], sub: List[B]): List[B] = (sup, sub) match {
      case (_, Nil) => Nil
      case (Cons(suph, supt), Cons(subh, subt)) if suph == subh => matchesSubsequence(supt, subt)
      case _ => sub
    }

    (sup, matchesSubsequence(sup, sub)) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(_, t), _) => hasSubsequence(t, sub)
    }
  }

}
