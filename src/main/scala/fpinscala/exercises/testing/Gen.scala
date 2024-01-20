package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

type TestCases = Int

enum Result:
  case Passed
  case Falsified(failure: FailedCase, successful: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed          => false
    case Falsified(_, _) => true

trait Prop {
  def check: TestCases => Result
  def &&(that: Prop): Prop
}

object Prop:
  opaque type FailedCase = String
  opaque type SuccessCount = Int
  opaque type Prop = (TestCases, RNG) => Result
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    (t, r) =>
      val (a, next) = gen.next(r)
      for (i <- 0 to t) {
        // gen next
        // run f
        // if true record result and i
        // if false return Falsified
        // in case of exception Falsified
      }
      Result.Passed

opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)

  def unit[A](a: => A): Gen[A] = State.unit(a)

  def choose(start: Int, stopInclusive: Int): Gen[Int] = {
    State(r => {
      r.nextInt match
        case (i, nextState) if (i > start && i <= stopInclusive) =>
          (i, nextState)
        case (i, nextState) => {
          // the following with some magical help
          val mod =
            i & Int.MaxValue // Ensure the result is positive and start at 0
          val range = stopInclusive - start + 1 // Calculate the range size
          val scaled =
            mod % range // Scale the result to the desired range size
          val result =
            scaled + start // Shift the scaled value to start at min
          (result, nextState)
        }
    })
  }

  def choose(r: Range): Gen[Int] = choose(r.start, r.end)

  // def boolean: Gen[Boolean] = choose(0, 1).map(s =>
  //   s match
  //     case i if i == 0 => true
  //     case _           => false
  // )

  def boolean: Gen[Boolean] = State(RNG.boolean)

  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] = State.sequence(List.fill(n)(self))

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)
    // State: s =>
    //   val (a, nextState) = self.run(s)
    //   val sb = f(a)
    //   sb.next(nextState)

  extension [A](self: Gen[A])
    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap {
      case true => g1
      case _    => g2
    }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    // this was hard to come to, there's no way to tell that the Double in the pair is a probability balanced
    // Tuple(n) gets the nth elem of the tuple
    val threshold = g1(1).abs / (g1(1).abs + g2(1).abs)
    State(RNG.double).flatMap(d => if d > threshold then g2(0) else g1(0))

  val ASCII_RANGE = 0 to 255
  def ascii(n: Int): Gen[String] =
    choose(ASCII_RANGE).map(_.toChar).listOfN(n).map(_.toString())

// trait Gen[A]:
//   def map[B](f: A => B): Gen[B] = ???
//   def flatMap[B](f: A => Gen[B]): Gen[B] = ???
//
trait SGen[+A]
