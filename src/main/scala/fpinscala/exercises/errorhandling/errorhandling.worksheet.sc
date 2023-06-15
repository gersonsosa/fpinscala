import scala.Option as _
import fpinscala.exercises.errorhandling.Option
import fpinscala.exercises.errorhandling.Option.*

val test = Some(2)
test.map { a => a * 2 }
test.getOrElse(3)

val n = None
n.getOrElse(2)

n.orElse(Some(5))

test.filter { t => t == 1 }

variance(List(1, 2, 3, 4, 5))

sequence(List(Some(1), Some(2), Some(3)))
traverse(List(1, 2, 3))(a => Some(a.toDouble))

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import fpinscala.exercises.errorhandling.Either
import fpinscala.exercises.errorhandling.Either.*

val e: Either[String, Int] = Right(123)
e.map(n => n * 10)
e.flatMap {
  case n: Int if n > 10 => Right(true)
  case _: Int           => Left("Not enough")
}

val el: Either[String, String] = Left("The other side")
el.getOrElse("Actual side")

el.orElse(Right("The good side"))

val b = Right(20)

e.map2(b)(_ + _)
