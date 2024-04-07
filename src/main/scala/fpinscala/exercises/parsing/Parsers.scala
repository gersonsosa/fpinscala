package fpinscala.exercises.parsing

import fpinscala.answers.testing.Gen
import fpinscala.answers.testing.Prop
import scala.util.matching.Regex
import scala.annotation.tailrec

trait Parsers[Parser[+_]]:
  self => // so inner classes may call methods of trait

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def string(s: String): Parser[String]

  // NOTE: this is required because the map impl uses succeed which creates a circular reference
  def defaultSucceed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def succeed[A](a: A): Parser[A]

  def fail(msg: String): Parser[Nothing]

  def sequence[A](l: List[Parser[A]]): Parser[List[A]]

  def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A])

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))

    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p(0)(0), p(0)(1), p(1))
    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p(0), p(1)(0), p(1)(1))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    // a ** (b ** c) equal (a ** b) ** c
    def associativity[A](p1: Parser[A], p2: Parser[A], p3: Parser[A])(
        in: Gen[String]
    ): Prop =
      // NOTE: product groups results in tuples but with a bias ((A, B), C) != (A, (B, C))
      equal((p1 ** (p2 ** p3)).map(unbiasR), ((p1 ** p2) ** p3).map(unbiasL))(
        in
      )

  extension [A](p: Parser[A])

    def run(input: String): Either[ParseError, A]

    def listOfN(n: Int): Parser[List[A]] =
      n match
        case i if i == 0 => succeed(Nil)
        case i if i > 0  => p.map2(p.listOfN(n - 1))((a, b) => a :: b)

    // listOfN based on sequence
    def _listOfN(n: Int): Parser[List[A]] = sequence(List.fill(n)(p))

    infix def or(other: => Parser[A]): Parser[A]
    def |(other: Parser[A]): Parser[A] = p.or(other)

    // NOTE: cannot use a for comprehension here it will cause a circular reference to map in the end
    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen succeed)

    def slice: Parser[String]

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      for {
        a <- p
        b <- p2
        ab <- succeed(a, b)
      } yield ab

    def **[B](p2: Parser[B]): Parser[(A, B)] = product(p2)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      for {
        a <- p
        b <- p2
      } yield f(a, b)

    def many: Parser[List[A]] = p.map2(p.many)((a, b) => a :: b) | succeed(Nil)

    def many1: Parser[List[A]] = p.map2(p.many)((a, b) => a :: b)

    def flatMap[B](f: A => Parser[B]): Parser[B]

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1

  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match
    case -1        => offset + 1
    case lineStart => offset - lineStart

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  def remaining: String = ???

  def slice(n: Int) = ???

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.linesIterator.drop(line - 1).next()
    else ""

case class ParseError(
    stack: List[(Location, String)] = List(),
    otherFailures: List[ParseError] = List()
):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] = regex("""^\d{1}""".r).flatMap { d =>
    d.toIntOption match
      case Some(value) => succeed(value)
      case _           => fail("cannot parse integer from" + d)
  }

  // NOTE:what is regex returning? assuming is the regex matched
  // use ** to combine regex and then verify the second string is the same char the number of the first
  val nConsecutiveAs: Parser[Int] =
    nonNegativeInt.map2(regex(""".*""".r)) { (d, s) =>
      def times(t: Int): Boolean = {
        val ref = s.charAt(0)
        s.length() == t && s.foldRight(true)((a, n) => ref == a && n)
      }
      if times(d) then d else 0
    }
