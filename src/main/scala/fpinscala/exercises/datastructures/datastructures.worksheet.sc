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
reverse(List(1,2,3,4,5,6,7,8,9))

reverseR(List(1,2,3,4,5,6,7,8,9))

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

filter(List(1, 2, 3, 4), (a) => a > 2)

flatMap(List(1, 2, 3), (a) => List(a, a))

filterViaFlatMap(List(1, 2, 3, 4, 5), (a) => a >= 3)

addPairwise(List(1, 2, 3), List(4, 5, 6))
zipWith(List(1, 2, 3), List(1, 2, 3), (a, b) => a + b)

hasSubsequence(List(1, 2, 3, 4), List(2, 3))
hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3))
hasSubsequence(List(1, 2, 3, 4), List(2, 4))

// Trees
import fpinscala.exercises.datastructures.Tree
import fpinscala.exercises.datastructures.Tree.Branch
import fpinscala.exercises.datastructures.Tree.Leaf

val t =
  Branch(
    Branch(
      Branch(
        Branch(Leaf(5), Leaf(0)),
        Leaf(1)
      ),
      Branch(Leaf(2), Leaf(3))
    ),
    Leaf(4)
  )
val t2 = Branch(Leaf(1), Leaf(2))
val t3 = Leaf(1)
t.size
t.maximum
t.depth
t2.depth
t3.depth

t.map(x => s"$x")
t.sizeViaFold
t.depthViaFold
t2.depthViaFold
t3.depthViaFold

t.mapViaFold(x => s"$x")