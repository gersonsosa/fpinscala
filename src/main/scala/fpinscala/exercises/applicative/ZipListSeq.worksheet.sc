import fpinscala.exercises.applicative.Applicative.ZipList

/**
 * NOTE: This will transpose/rotate the lists
 * 1,2,3
 * 4,5,6
 * 7,8,9
 * will become
 * 1,4,7
 * 2,5,8
 * 3,6,9
 */
def sequence[A](as: List[ZipList[A]]): ZipList[List[A]] = // List(ZL(1,2,3), ZL(4,5,6)...)
  as.foldRight(ZipList.zipListApplicative.unit(List[A]())) { (e, acc) =>
    e.map2(acc)(_ :: _) // e=ZL(1,2,3) acc=ZL() => List(1),List(2),List(3)
  } // e=ZL(4,5,6) acc=List(List(1), List(2), List(3)) => List(List(1, 4), List(2, 5), List(3, 6))
// e=ZL(4,5,6) acc=List(List(1), List(2), List(3)) => List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))

val z1 = ZipList.fromLazyList(LazyList(1, 2, 3))
val z2 = ZipList.fromLazyList(LazyList(4, 5, 6))
val z3 = ZipList.fromLazyList(LazyList(7, 8, 9))

val s = sequence(List(z1, z2, z3))

s.toLazyList.foreach(println)
