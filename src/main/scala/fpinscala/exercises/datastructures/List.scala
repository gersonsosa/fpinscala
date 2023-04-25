package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
    // the result is 3, the match goes to the third branch

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => Nil // I think this is better than throwing an exception
      case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] =
    n match
      case 0 => l
      case i if i < 0 => l
      // case i if i > length(l) => Nil
      case _ => drop(tail(l), n - 1)

  /**
   * When it encounters the first element that doesn't match the predicate
   * returns the rest.
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l

  def init[A](l: List[A]): List[A] =
    l match
      case Cons(h, t) if t != Nil => Cons(h, init(t))
      case _ => Nil

  def length[A](l: List[A]): Int = foldRight(l, 0, (_, acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)

  // val l = List(1,2,3,4)
  // foldLeft(List(1,2,3,4), 0, _ + _)
  // foldLeft(List(2,3,4), 1, _ + _)
  // foldLeft(List(3,4), 3, _ + _)
  // foldLeft(List(4), 6, _ + _)
  // foldLeft(Nil, 10, _ + _)
  //
  // foldRight(List(1,2,3,4), 0, _ + _)
  // 1 + foldRight(List(2,3,4), 0, _ + _)
  // 1 + 2 + foldRight(List(3,4), 0, _ + _)
  // 1 + 2 + 3 + foldRight(List(4), 0, _ + _)
  // 1 + 2 + 3 + 4 + foldRight(Nil, 0, _ + _)
  // 1 + 2 + 3 + 4 + 0
  // 1 + 2 + 3 + 4
  // 1 + 2 + 7
  // 1 + 9
  // 10

  def foldRight1[A, B](
      as: List[A],
      acc: B,
      f: (A, B) => B
  ): B = // Utility functions
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  // List(1,2,3) => List(3,2,1)
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A](), (acc, head) => Cons(head, acc))

  // reverse in terms of foldRight
  // this is O(n^2) complexity
  def reverseR[A](l: List[A]): List[A] = foldRight(
    l,
    List[A](),
    (head, acc) => append(acc, Cons(head, Nil))
  )

  def foldLeftFromRight[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    foldRight(l, acc, (a, b) => f(b, a))

  def foldRightFromLeft[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(l, acc, (x, y) => f(y, x))

  def foldRightViaFoldLeft_1[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    val accum: (B => B, A) => (B => B) = (g, a) => b => g(f(a, b))
    foldLeft(l, (b: B) => b, accum)(acc)
  // foldLeft(List(1,2,3), (x: B) => x, (g, a) => x => g(a + x))
  // foldLeft(List(2,3), x => a + x, (g, a) => x => 1 + x)
  // foldLeft(List(3), x => g(a + x), (g, a) => x => 2 + 1 + x)
  // foldLeft(Nil, x => g(a + x), (g, a) => x => 3 + 2 + 1 + x)
  // here x is the inital accumulator = 0

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, (a, b) => Cons(a, b))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = foldLeft(reverse(l), r, (a, b) => Cons(b, a))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, List[A](), appendViaFoldLeft)

  def concat2[A](l: List[List[A]]): List[A] = foldLeft(l, List[A](), append)

  def incrementEach(l: List[Int]): List[Int] = foldRight(l, List[Int](), (i, acc) => Cons(i + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, List[String](), (c, acc) => Cons(c.toString(), acc))

  def map[A,B](l: List[A], f: A => B): List[B] = foldRight(l, List[B](), (a, b) => Cons(f(a), b))

  // ListBuffer operations amortize adding and converting toList constant time, not sure how
  def map2[A, B](l: List[A], f: A => B): List[B] =
    val buffer = new collection.mutable.ListBuffer[B]()
    def iter(current: List[A]): Unit = {
      current match
        case Nil => ()
        case Cons(h, t) => buffer += f(h); iter(t)
    }
    iter(l)
    List(buffer.toList*)

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    val buffer = new collection.mutable.ListBuffer[A]()
    def iter(current: List[A]): Unit = {
      current match
        case Nil => ()
        case Cons(h, t) if f(h) => buffer += h; iter(t)
        case Cons(_, t) => iter(t)
    }
    iter(as)
    List(buffer.toList*)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = foldRight(as, List[B](), (i, acc) => append(f(i), acc))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = flatMap(as, (a) => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Cons(aHead + bHead, addPairwise(aTail, bTail))
      case _ => Nil
    

  // def zipWith - TODO determine signature
  // note that the lists a,b do not have to be the same type the same goes for the resulting list
  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Cons(f(aHead, bHead), zipWith(aTail, bTail, f))
      case _ => Nil

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    @tailrec
    def startsWith[A](l: List[A], test: List[A]): Boolean = 
      (l, test) match
        case (_, Nil) => true
        case (Cons(headSup, tailSup), Cons(headSub, tailSub)) if headSup == headSub => startsWith(tailSup, tailSub) 
        case _ => false
    sup match
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, tail) => hasSubsequence(tail, sub)
