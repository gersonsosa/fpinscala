import fpinscala.exercises.laziness.*

val ll = LazyList((1 to 10)*)
ll.toList
ll.take(1).toList
ll.take(2).toList
ll.take(3).toList
ll.take(12).toList

ll.drop(1).toList
ll.drop(2).toList
ll.drop(3).toList

ll.takeWhile((e) => e < 5).toList
ll.takeWhile((e) => e < 10).toList
ll.takeWhile((e) => e > 9).toList

ll.takeWhileNaive((e) => e < 5).toList
ll.takeWhileNaive((e) => e < 10).toList
ll.takeWhileNaive((e) => e > 9).toList

ll.forAll((elem) => elem > 0)
ll.forAll((elem) => elem < 0)

ll.headOption

LazyList.fibs.take(10).toList

LazyList.fromViaUnfold(10).take(10).toList
