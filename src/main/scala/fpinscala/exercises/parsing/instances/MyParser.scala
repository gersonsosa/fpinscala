import fpinscala.exercises.parsing.Parsers
import fpinscala.exercises.parsing.Location
import fpinscala.exercises.parsing.ParseError
import scala.util.matching.Regex

object MyParser extends Parsers[MyParser.ParserAttempt]:

  type ParserAttempt[+A] = Location => Result[A]

  enum Result[+A]:
    case Success(get: A, length: Int)
    case Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

    def mapError(f: ParseError => ParseError): Result[A] = this match
      case Failure(e, isCommitted) => Failure(f(e), isCommitted)
      case _ => this

    def uncommit: Result[A] = this match
      case Failure(e, isCommitted) => Failure(e, false)
      case _ => this

    def addCommit(isCommitted: Boolean): Result[A] = this match
      case Failure(e, alreadyCommitted) => Failure(e, alreadyCommitted || isCommitted)
      case _ => this

    def advanceSuccess(n: Int): Result[A] = this match
      case Success(a, consumed) => Success(a, consumed + n)
      case _ => this

  def string(s: String): ParserAttempt[String] =
    loc =>
      loc match
        case Location(input, _) if s == input => Result.Success(s, s.length)
        case _ => Result.Failure(loc.toError(s"cannot match $s"), true)

  def regex(r: Regex): ParserAttempt[String] =
    loc =>
      r.findPrefixOf(loc.remaining) match
        case Some(m) => Result.Success(m, m.length)
        case None => Result.Failure(loc.toError(s"regex $r didn't match input ${loc.remaining}"), true)

  def succeed[A](a: A): ParserAttempt[A] = l => Result.Success(a, 0)

  def fail(msg: String): MyParser.ParserAttempt[Nothing] = ???

  def sequence[A](l: List[MyParser.ParserAttempt[A]]): MyParser.ParserAttempt[List[A]] = ???

  extension [A](p: ParserAttempt[A])
    def slice: ParserAttempt[String] =
      loc =>
        p(loc) match // to optimize further don't run it on the whole input, but that will require additional changes
          case Result.Success(_, n) => Result.Success(loc.slice(loc.offset + n), n)
          case f @ Result.Failure(e,_) => f

    def scope(msg: String): ParserAttempt[A] = loc => p(loc).mapError(_.push(loc, msg))

    def label(msg: String): ParserAttempt[A] = loc => p(loc).mapError(_.label(msg))

    def attempt: ParserAttempt[A] = l => p(l).uncommit

    def or(other: => ParserAttempt[A]): ParserAttempt[A] = loc => p(loc) match
      case Result.Failure(get, false) => other(loc)
      case r => r

    def flatMap[B](f: A => ParserAttempt[B]): ParserAttempt[B] =
      loc =>
        p(loc) match
          case Result.Success(a, consumed) => f(a)(loc.advanceBy(consumed))
            .addCommit(consumed != 0)
            .advanceSuccess(consumed)
          case f @ Result.Failure(_, _) => f

    def run(input: String): Either[ParseError, A] = ???

