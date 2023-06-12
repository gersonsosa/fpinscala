import fpinscala.exercises.errorhandling.Option.*

val test = Some(2)
test.map { a => a * 2 }
test.getOrElse(3)

val n = None
n.getOrElse(2)

n.orElse(Some(5))

test.filter { t => t == 1 }

variance(List(1,2,3,4,5))

sequence(List(Some(1), Some(2), Some(3)))
traverse(List(1, 2, 3))(a => Some(a.toDouble))
