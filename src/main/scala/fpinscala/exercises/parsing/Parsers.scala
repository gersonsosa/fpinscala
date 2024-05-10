package fpinscala.exercises.parsing

import fpinscala.answers.testing.Gen
import fpinscala.answers.testing.Prop
import scala.util.matching.Regex
import scala.annotation.tailrec

trait Parsers[Parser[+_]]:
  self => // so inner classes may call methods of trait

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def string(s: String): Parser[String]
  def letter = regex("a-zA-Z".r)
  def digit = regex("""\d""".r)
  def whitespace = regex("""\s""".r)

  def surrounded(c: Char): Parser[String] = for {
    _ <- char(c)
    s <- regex(".*".r)
    _ <- char(c)
  } yield s

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

    // NOTE: do not commit to this parser by default, just try it if it doesn't
    // work bias towards alternatives
    def attempt: Parser[A]

    def listOfN(n: Int): Parser[List[A]] =
      n match
        case i if i <= 0 => succeed(Nil)
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

    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def *>[B](p2: => Parser[B]): Parser[B] = p.map2(p2)((_, b) => b)
    def <*[B](p2: => Parser[B]): Parser[A] = p.map2(p2)((a, _) => a)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      for {
        a <- p
        b <- p2
      } yield f(a, b)

    /*
     * strips the whitespace surrounding the parser
     */
    def strip = p.map2(whitespace) { (a, b) => a}

    def many: Parser[List[A]] = p.map2(p.many)((a, b) => a :: b) | succeed(Nil)

    def many1: Parser[List[A]] = p.map2(p.many)((a, b) => a :: b)

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def label(msg: String): Parser[A]
    def scope(msg: String): Parser[A]

// NOTE: if this parses the whole input every time it appears to be inefficient
// mostly on calculating the line and col for large inputs.
// on the other side if this holds just a small part of the input
// it will explode the amount of memory on duplicating the string for all the
// locations needed while parsing
case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1

  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match
    case -1        => offset + 1
    case lineStart => offset - lineStart

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  def remaining: String = input.substring(offset)

  def slice(n: Int) = input.slice(offset, n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.linesIterator.drop(line - 1).next()
    else ""

case class ParseError(
    stack: List[(Location, String)] = List(),
    otherFailures: List[ParseError] = List()
):
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)

  def label(msg: String): ParseError = ParseError(latestOrNone.map((_, msg)).toList)

  def latestOrNone: Option[Location] = stack.lastOption.map(_(0)) // get only the Location

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] = for {
    d <- regex("""^\d+""".r).label("starts with an integer")
    i <- d.toIntOption match {
      case Some(v) => succeed(v)
      case _       => fail(s"cannot parse integer from $d")
    }
  } yield i

  val nConsecutiveAs: Parser[Int] =
    for {
      n <- nonNegativeInt
      _ <- char('a').listOfN(n)
    } yield n

  val nConsecutiveChars: Parser[Int] =
    for {
      n <- nonNegativeInt
      s <- regex(".".r) // next char
      _ <- char(s.charAt(0)).listOfN(n)
    } yield n

  // NOTE:what is regex returning? assuming is the regex matched
  // use map2 to combine with regex and then verify the second string is the same char the number of the first
  val _nConsecutiveChars: Parser[Int] =
    nonNegativeInt.map2(regex(""".*""".r)) { (d, s) =>
      def times(t: Int): Boolean = {
        val ref = s.charAt(0)
        s.length() == t && s.foldRight(true)((a, n) => ref == a && n)
      }
      if times(d) then d else 0
    }
