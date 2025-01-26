package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{endoMonoid, dual}

  extension [A](as: F[A])
    // these implementations are just place holders
    // the type should override them, can this actually produce stack overflow?
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      as.foldLeft(acc)((b, a) => f(a, b))

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      as.foldRight(acc)((a, b) => f(b, a))

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      as.foldLeft(mb.empty)((b, a) => mb.combine(b, f(a)))

    def combineAll(using ma: Monoid[A]): A =
      as.foldLeft(ma.empty)(ma.combine)

    def toList: List[A] =
      as.foldLeft(Nil)((l, e) => e :: l).reverse

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        var res = acc
        var l = as.reverse // the expectation os that operation is applied from the tail to the head
        while (!l.isEmpty)
          res = f(l.head, res)
          l = l.tail
        res

      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        var res = acc
        var l = as
        while (!l.isEmpty) {
          res = f(res, l.head)
          l = l.tail
        }
        res

      override def toList: List[A] =
        as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        val iter = as.reverseIterator
        var res = acc
        while (iter.hasNext)
          res = f(iter.next, res)
        res

      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        val iter = as.iterator
        var res = acc
        while (iter.hasNext)
          res = f(res, iter.next)
        res

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        val iter = as.iterator
        var acc = mb.empty
        while (iter.hasNext)
          acc = mb.combine(acc, f(iter.next))
        acc

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      // a lazy list will be computed at some point so this approach doesn't seem to have any disadvantage
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        var l = as.reverse
        var res = acc
        while (l.isEmpty)
          res = f(l.head, acc)
          l = l.tail
        res

      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        var l = as
        var res = acc
        while (!l.isEmpty)
          res = f(res, l.head)
          l = l.tail
        res

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      // folding to the right should start grouping the right first
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as match
          case Leaf(value) => f(value, acc)
          case Branch(left, right) => left.foldRight(right.foldRight(acc)(f))(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as match
          case Leaf(value) => f(acc, value)
          case Branch(left, right) => right.foldLeft(left.foldLeft(acc)(f))(f)

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as match
          case Leaf(v) => mb.combine(f(v), mb.empty)
          case Branch(l, r) => mb.combine(l.foldMap(f), r.foldMap(f))

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as match
          case Some(v) => f(v, acc)
          case _ => acc

      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as match
          case Some(v) => f(acc, v)
          case _ => acc

      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as match
          case Some(v) => mb.combine(f(v), mb.empty)
          case _ => mb.empty
