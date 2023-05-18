package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(value)         => 0
    case Branch(left, right) =>  (1 + left.depth) max (1 + right.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(value) => g(f(value), f(value))
    case Branch(left, right) => g(left.fold(f, g), right.fold(f, g))

  def sizeViaFold: Int = fold(x => 0, (a, b) => 1 + a + b)

  def depthViaFold: Int = fold(x => 0, (a, b) => (1 + a) max (1 + b))

  def mapViaFold[B](f: A => B): Tree[B] = fold(x => Leaf(f(x)), (a, b) => Branch(a, b))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = ???

  extension (t: Tree[Int]) def maximum: Int =
    t match
      case Leaf(value) => value
      case Branch(left, right) => left.maximum max right.maximum
    

  extension (t: Tree[Int]) def maximumViaFold: Int = ???
