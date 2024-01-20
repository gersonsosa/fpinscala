import fpinscala.answers.parallelism.Examples.sum
import java.util.concurrent.Executors
import fpinscala.exercises.parallelism.Par.countWords
import fpinscala.exercises.parallelism.* 

val p = List("this is a paragraph I want to count the words in", "let me add another for the sake of the test", "and the last one the classic foo bar baz one more")

val es = Executors.newFixedThreadPool(2)
val f = countWords(p).run(es)
f.get()

val g = Par.apply(p)(l => l.split(" ").length)(_.sum).run(es)
g.get()

val pa = Par.unit(1)
val pb = Par.unit(2)
val pc = Par.unit(2)
val pd = Par.unit(2)

pa.map3(pb)(pc)((a, b, c) => a + b + c).run(es)
pa.map4(pb)(pc)(pd)((a, b, c, d) => a + b + c + d).run(es)

val a = Par.lazyUnit(42 + 1)
// val es1 = Executors.newFixedThreadPool(1), will deadlock
println(Par.equal(es)(a, Par.fork(a)))
