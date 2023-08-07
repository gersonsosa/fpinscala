import fpinscala.exercises.state.RNG.Simple
import fpinscala.exercises.state.*

val initialSeed = Simple(1L)
val (n, next) = RNG.nonNegativeInt(initialSeed)
assert(n > 0)

val (d, nextDouble) = RNG.double(initialSeed)
assert(d >= 0)
assert(d < 1)

RNG.ints(4)(initialSeed)
