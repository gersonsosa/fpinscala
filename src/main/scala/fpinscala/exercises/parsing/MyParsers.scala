/* This is my take on the Section 9.1 ex */
package fpinscala.exercises.parsing

// how to define fields for the type ParserError in Parsers?
type ParserError = {
  // byte offset position where the error ocurred
  // not sure if byte is a good type, cannot think of a better one
  val position: Byte
  // until what point are we going to hightligh the error when rendering
  val span: Int
  val message: String
  val suggestion: String
}

trait Parsers[ParserError, Parser[+_]]:
  def char(c: Char): Parser[Char]
  def string(s: String): Parser[String]

  extension [A](p: Parser[A])
    def run(input: String): Either[ParserError, A]

    infix def or(other: Parser[A]): Parser[A]
    def |(other: Parser[A]): Parser[A] = p.or(other)

    // would the and & combinator be useful to define?
    // a parser that recognizes a character followed by another? there seems to be
    // uses for a "continuation" of parsers, in json we should be able to define
    // a way to recognize '{' followed by '"' or a ':' after a '"' etc.

    def listOfN(n: Int): Parser[List[A]]

    // counts the occurrences of a in p, we can define a new function
    def count(a: A): Parser[A]
    // there is a way to define this in terms of map or simply foldRight
    // in that sense map and foldRight and is a more primitive building block
    def map[B](f: A => B): Parser[B]
    def foldRight[B](acc: B)(f: (B, A) => B): Parser[B]

    // it seems too specific to define a count of charachers as Int
    // what if you have more than Int.MaxValue?
    // however returning the list of matching A will potentially duplicate
    // the memory in the worst case
    // can we get away with a type bound for numeric values?
    def countByMap(a: A): Parser[Int] = p.foldRight(0) { (num, c) =>
      if (c == a) then num + 1 else num
    }

    // matches the character a in parser or returns an error
    def countMatching(a: A): Either[Parser[A], ParserError]
    // this feels like it can be define in terms of another more basic operations
    // for example a map(a => a == 'a').find(false)
    // or a foldRight that can return either the acc or a ParserError
    // the latter can be more efficient if the first char is not the one wanted

    // a parser that given "aa" or "b" returns the count in a pair (2, 1)
    // If we use countMatching for a when we get an error countMatching for b we can construct the answer
    def countPair(a: A, b: A): (Parser[Int], Parser[Int])

    // match, for a given Parser[A] and a character "a" advance until the character
    // no longer matches and return last known position of "a".

  // Regardless of the internal representation of ParserError this will render
  // the error to the user
  extension (e: ParserError) def render(): String = ""
