package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.cons
import fpinscala.exercises.laziness.LazyList.empty
import fpinscala.exercises.laziness.LazyList.unfold

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match
      // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    case Empty      => None

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    // we need this branch to avoid evaluation of the tail when n == 1
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _                    => empty

  @annotation.tailrec // must set the function as final or private to avoid runtime issues
  final def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  def takeWhileNaive(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((elem, acc) => p(elem) && acc)

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight(Empty: LazyList[A])((elem, acc) =>
      if (p(elem)) cons(elem, acc) else empty
    )

  def headOption: Option[A] = foldRight(None: Option[A])((e, _) => Some(e))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): LazyList[B] =
    foldRight(empty)((e, acc) => cons(f(e), acc))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty)((e, acc) => if (f(e)) cons(e, acc) else acc)

  def append[B >: A](another: LazyList[B]): LazyList[B] =
    // we start with `another` becuase the return type must be `LazyList[B]`
    foldRight(another)((e, acc) => cons(e, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty)((e, acc) => f(e).append(acc))

  def startsWith[B](s: LazyList[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll((a, b) => a == b)

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this)(l =>
      l match
        case Cons(h, t) => Some(f(h()), t())
        case _          => None
    )

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) { case (l, c) =>
      (l, c) match
        case (Cons(h, t), 1)          => Some(h(), (empty, 0))
        case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
        case _                        => None
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this)(l =>
      l match
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _                    => None
    )

  def zipWith[B, C](other: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, other)) { case (a, b) =>
      (a, b) match
        case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
        case _                            => None
    }

  def zipAll[B](other: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, other)) { case (a, b) =>
      (a, b) match
        case (Cons(ha, ta), Cons(hb, tb)) =>
          Some((Some(ha()), Some(hb())), (ta(), tb()))
        case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (empty, tb()))
        case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), empty))
        case _                     => None
    }

  def tails: LazyList[LazyList[A]] =
    unfold((true, this)):
      case (true, Cons(h, t)) => Some(cons(h(), t()), (true, t()))
      case (true, Empty) => Some(empty, (false, empty))
      case _ => None

  def hasSubsequence[A](s: LazyList[A]): Boolean =
    tails.exists(_.startsWith(s))

  def scanRight[B](z: => B)(f: (A, => B) => B): LazyList[B] =
    tails.map(_.foldRight(z)(f))

object LazyList:

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = {
    def rec(f: Int, s: Int): LazyList[Int] = {
      cons(f, rec(s, f + s))
    }
    rec(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((v, s)) => LazyList.cons(v, unfold(s)(f))
      case None         => empty

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1)) { case (first, second) =>
      Some(first, (second, first + second))
    }

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n) { n =>
    Some(n, n + 1)
  }

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(a) { a =>
    Some(a, a)
  }

  lazy val onesViaUnfold: LazyList[Int] = unfold(1)(_ => Some(1, 1))
