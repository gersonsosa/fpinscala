package fpinscala.exercises.parsing

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

/*
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" :
    [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
 */
object JSON:
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*

    def element(e: String) = string(e).strip

    def jwhitespace = regex("[ \t\n\r]+".r)

    def exp = char('e').map2(char('-') | char('+')) { (e, s) => s"e$s" }

    def jnumber = for {
      s <- char('-')
      d <- digit.many1
      c <- string(".") | exp
      e <- digit.many1
    } yield JNumber(s"$s$d$c$e".toDouble)

    def bool = for {
      v <- string("true") | string("false")
    } yield JBool(v.toBoolean)

    def esc = char('\\').map2(char('"') | char('\\') | char('/')) { (_, ch) =>
      s"\\$ch"
    }

    def jstring = for {
      _ <- element(""""""")
      s <- (regex("\\w".r) | esc).strip.many1.slice
      _ <- element(""""""")
    } yield JString(s)

    def jval: Parser[JSON] =
      (jstring | jnumber | obj | array | bool | succeed(JNull)).strip

    def array: Parser[JSON] = for {
      _ <- element("[")
      c <- (jval | element(",").map(JString(_))).strip.many1
      _ <- element("[")
    } yield JArray(IndexedSeq.from(c))

    def prop = (regex("\\w".r) | esc).strip
      .map2(char(':')) { (name, _) => name }
      .map2(jstring) { (a, b) => (a -> b) }

    def obj: Parser[JSON] = for {
      _ <- element("{")
      props <- prop.many
      _ <- element("{")
    } yield JObject(Map.from(props))

    obj
