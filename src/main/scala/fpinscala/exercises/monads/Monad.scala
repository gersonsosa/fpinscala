package fpinscala.exercises
package monads

import parsing.*
import testing.*
import parallelism.*
import state.*
import parallelism.Par.*

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)]) def distribute: (F[A], F[B]) =
    (fab.map(_(0)), fab.map(_(1)))

  extension [A, B](e: Either[F[A], F[B]]) def codistribute: F[Either[A, B]] =
    e match
      case Left(fa) => fa.map(Left(_))
      case Right(fb) => fb.map(Right(_))

object Functor:
  given listFunctor: Functor[List] with
    extension [A](as: List[A])
      def map[B](f: A => B): List[B] = as.map(f)

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A]))((fa, acc) => fa.map2(acc)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, acc) => f(a).map2(acc)(_ :: _))

  // NOTE: will create a list of F which can be a List[List[A]]
  // this will invoke flatMap which creates a new List/LazyList
  // and fills it with the given a (which will be references to the original)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    fa.map(a => List.fill(n)(a))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  extension [A](fa: F[A])
    def flatMapViaCompose[B](f: A => F[B]): F[B] =
      // NOTE: is tricky to think this doesn't stack overflows, you have to be careful to implement flatMap or map
      compose(_ => fa, f)(())

  // NOTE: builds a Monad of elements that satisfies the function a -> F[Boolean]
  // Option of list with elements that satisfies f
  // List of list with elements that satisfies f
  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List.empty[A]))((a, acc) => f(a).flatMap(bool => if bool then unit(a).map2(acc)(_ :: _) else acc))

  extension [A](ffa: F[F[A]]) def join: F[A] =
    ???

  extension [A](fa: F[A])
    def flatMapViaJoinAndMap[B](f: A => F[B]): F[B] =
      ???

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    ???

end Monad

object Monad:
  given genMonad: Monad[Gen] with
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    extension [A](fa: Gen[A])
      override def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(fa)(f)

  given parMonad: Monad[Par] with
    def unit[A](a: => A) = Par.unit(a)
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        fa.chooser(f)

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new:
    def unit[A](a: => A) = p.succeed(a)
    extension [A](fa: P[A])
      override def flatMap[B](f: A => P[B]): P[B] =
        p.flatMap(fa)(f) // the flatMap implementation is defined in a extension that takes a parser

  given optionMonad: Monad[Option] with
    def unit[A](a: => A) = Some(a)
    extension [A](fa: Option[A])
      override def flatMap[B](f: A => Option[B]) =
        fa.flatMap(f) // the implementation inlines

  given lazyListMonad: Monad[LazyList] with
    def unit[A](a: => A) = LazyList(a)
    extension [A](fa: LazyList[A])
      override def flatMap[B](f: A => LazyList[B]) =
        fa.flatMap(f) // the implementation of LazyList.flatMap is not trivial

  given listMonad: Monad[List] with
    def unit[A](a: => A) = List(a)
    extension [A](fa: List[A])
      override def flatMap[B](f: A => List[B]) =
        fa.flatMap(f) // internally uses releaseFence so no trivial

  // NOTE: define the type S over the whole monad was this too simple?
  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A) = State.unit(a)
    extension [A](fa: State[S, A])
      override def flatMap[B](f: A => State[S, B]) =
        // flatMap is defined in an extension that takes an underlying state
        State.flatMap(fa)(f)

end Monad

case class Id[+A](value: A):
  def map[B](f: A => B): Id[B] =
    ???
  def flatMap[B](f: A => Id[B]): Id[B] =
    ???

object Id:
  given idMonad: Monad[Id] with
    def unit[A](a: => A) = ???
    extension [A](fa: Id[A])
      override def flatMap[B](f: A => Id[B]) =
        ???

opaque type Reader[-R, +A] = R => A

object Reader:
  extension [R, A](ra: Reader[R, A])
    def run(r: R): A = ra(r)

  given readerMonad[R]: Monad[Reader[R, _]] with
    def unit[A](a: => A): Reader[R, A] = ???
    extension [A](fa: Reader[R, A])
      override def flatMap[B](f: A => Reader[R, B]) =
        ???
