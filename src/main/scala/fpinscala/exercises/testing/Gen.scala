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

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  opaque type FailedCase = String
  object FailedCase:
    extension (s: FailedCase) def +(that: String): FailedCase = s.+(that)
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  opaque type SuccessCount = Int
  object SuccessCount:
    extension (s: SuccessCount) def +(that: Int): SuccessCount = s.+(that)
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successful: SuccessCount)

    def isFalsified: Boolean = this match
      case Passed          => false
      case Falsified(_, _) => true

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop =
    def formatFailedCase(a: A, maybeEx: Option[Exception] = None): String =
      maybeEx match
        case Some(ex) =>
          s"Test case $a failed \n" +
            s"generated an exception: ${ex.getMessage()} \n" +
            s"with stack trace: ${ex.getStackTrace.mkString("\n")}"
        case _ => s"Test case $a failed"

    Prop.apply((t, r) => // why this works, shouldn't MaxSize be required here?
      for (i <- 0 to t.toInt) {
        gen.next(r) match
          case (a, n) =>
            try
              if (f(a)) then Result.Passed
              else Result.Falsified(formatFailedCase(a), i)
            catch
              case e: Exception =>
                Result.Falsified(formatFailedCase(a, Some(e)), i)
      }
      Result.Passed
    )

  @annotation.targetName("forAllSized")
  def forAll[A](sgen: SGen[A])(f: A => Boolean): Prop =
    (max, t, r) =>
      val cases = t.toInt / max.toInt
      for (i <- 0 to max.toInt) {
        for (j <- 0 to cases) {
          // ???
        }
      }
      Result.Passed

  extension (self: Prop)
    def &&(that: Prop): Prop =
      (max, t, r) =>
        (self(max, t, r), that(max, t, r)) match
          case (Result.Falsified(a, b), _) => Result.Falsified("Left " + a, b)
          case (_, Result.Falsified(a, b)) => Result.Falsified("Right " + a, b)
          case _                           => Result.Passed

  extension (self: Prop)
    def ||(that: Prop): Prop =
      (max, t, r) =>
        (self(max, t, r), that(max, t, r)) match
          case (Result.Falsified(a, b), Result.Falsified(c, d)) =>
            Result.Falsified(a + c, b + d)
          case _ => Result.Passed

  extension (self: Prop)
    def check(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Result =
      self(maxSize, testCases, rng)

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

  // does this implementation ensures fairness between true and false?
  // def boolean: Gen[Boolean] = choose(0, 1).map(s =>
  //   s match
  //     case i if i == 0 => true
  //     case _           => false
  // )

  def boolean: Gen[Boolean] = State(RNG.boolean)

  extension [A](self: Gen[A]) def unsized: SGen[A] = i => self

  extension [A](self: Gen[A]) def list: SGen[List[A]] = i => self.listOfN(i)

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

  val ASCII_RANGE = 32 to 126 // space, numbers letters and symbols
  def ascii(n: Int): Gen[String] =
    choose(ASCII_RANGE).map(_.toChar).listOfN(n).map(_.toString())

opaque type SGen[+A] = Int => Gen[A]

object SGen:
  def unit[A](a: => A): SGen[A] = i => Gen.unit(a)

  extension [A](self: SGen[A])
    def flatMap[B](f: A => SGen[B]): SGen[B] = i =>
      self(i).flatMap(a => f(a)(i))
