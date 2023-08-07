package fpinscala.exercises.state

trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match
      case (n, newRng) if n < 0 => ((n + 1) * -1, newRng)
      case n                    => n

  def double(rng: RNG): (Double, RNG) =
    val (n, next) = nonNegativeInt(rng)
    val c = 1 / (Int.MaxValue.toDouble + 1)
    (n * c, next)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (n, next) = nonNegativeInt(rng)
    val (d, next2) = double(next)
    ((n, d), next2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (n, next) = rng.nextInt
    val (d, next2) = double(next)
    ((d, n), next2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d, next) = double(rng)
    val (d1, next1) = double(next)
    val (d2, next2) = double(next1)
    ((d, d1, d2), next2)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    def iter(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) =
      if c <= 0 then (acc, r)
      else
        val (n, next) = r.nextInt
        iter(c - 1, next, n :: acc)
    iter(count, rng, List())

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, next) = ra(rng)
      val (b, nextB) = rb(next)
      (f(a, b), nextB)
    }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight[Rand[List[A]]](unit(Nil)) { (a, acc) =>
      map2(a, acc)(_ :: _)
    }

  // at first I used (List[Int], RNG) as a return type
  // is hard to get used to the aliases
  def intsViaSeq(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, next) = r(rng)
      val rb = f(a)
      rb(next)
    }

  // feels strange to use unit, it seems like the RNG in the second parentheses
  // comes from nowhere
  // on a second look, the RNG comes from the original r, it's just passed along
  // and specifically that one comes from the flatMap definition
  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s => {
        val (a, next) = underlying(s)
        (f(a), next)
      }

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s => {
        val (a, s1) = underlying(s)
        val (b, s2) = sb(s1)
        (f(a, b), s2)
      }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => {
        val (a, s1) = underlying(s)
        val sb = f(a)
        sb(s1)
      }

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    ls.foldRight[State[S, List[A]]](unit(Nil)) { (s, acc) =>
      s.map2(acc)(_ :: _)
    }

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
