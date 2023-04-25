import fpinscala.exercises.datastructures.List
import fpinscala.exercises.datastructures.List.*

val list = List(1, 2, 3, 4, 5)
tail(list)
setHead(list, 0)
setHead(Nil, 0)
drop(list, 100)
dropWhile(list, a => a < 4)
init(list)
init(List(Nil))
length(list)
length(List(1, 2, 3))

foldLeft(List(1, 2, 3, 4), 1, (x, y) => x * y)

sumViaFoldLeft(List(2, 3, 4))
productViaFoldLeft(List(2, 3, 4))
lengthViaFoldLeft(List(1))
lengthViaFoldLeft(List(1, 2, 3, 4, 5, 6))

reverse(List(1, 2, 3))
reverse(List("A", "B", "C"))

foldLeftFromRight(List(1, 2, 3, 4, 5), 0, _ + _)
foldLeftFromRight(List(1, 2, 3, 4, 5), 1, _ * _)
foldRightFromLeft(List(1, 2, 3, 4, 5), 0, _ + _)
foldRightFromLeft(List(1, 2, 3, 4, 5), 1, _ * _)

appendViaFoldRight(List(1, 2), List(3, 4))
appendViaFoldLeft(List(1, 2, 3, 4), List(5, 6, 7, 8, 9))

concat(List(List(1, 2), List(3, 4), List(5, 6)))

incrementEach(List(1, 2, 3, 4, 5))

doubleToString(List(1.2, 1.3, 1.4, 1.5))

map(List("1", "2", "3"), _.toInt)
map2(List("1", "2", "3"), _.toInt)

filter(List(1,2,3,4), (a) => a > 2)

flatMap(List(1,2,3), (a) => List(a, a))

filterViaFlatMap(List(1,2,3,4,5), (a) => a >= 3)

addPairwise(List(1,2,3), List(4,5,6))