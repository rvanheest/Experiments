import experiments.tryMonad.Try

import scala.language.postfixOps

val s1 = Try(1)
val s2 = Try(2)
val e1 = Try.error[Int](new IllegalArgumentException("foobar"))
val e2 = Try.error[Int](new NoSuchElementException("test123"))

s1.getOrElse(5)
s2.getOrElse(5)
e1.getOrElse(5)

s1.getOrCatch(_.getMessage.length)
s2.getOrCatch(_.getMessage.length)
e1.getOrCatch(_.getMessage.length)

s1.orElse(Try(3))
s2.orElse(Try(3))
e1.orElse(Try(3))

s1.orElse(throw new Exception("error on purpose"))
e1.orElse(throw new Exception("error on purpose"))

s1.recoverWith { case e => s2 }
e1.recoverWith { case e => s2 }

s1.recover { case e => 2 }
e1.recover { case e => 2 }

s1.ifSuccess(println)
e1.ifSuccess(println)

s1.flatMap(i => s2.flatMap(j => Try(i + j)))
s1.flatMap(i => e1.flatMap(j => Try(i + j)))
e1.flatMap(i => s2.flatMap(j => Try(i + j)))
e1.flatMap(i => e2.flatMap(j => Try(i + j)))

s1.map(8 *)
e1.map(8 *)

s1.map(i => s2.map(i +))
s1.map(i => e1.map(i +))
e1.map(i => s2.map(i +))
e1.map(i => e2.map(i +))

s1.map(i => s2.map(i +)).flatten
s1.map(i => e1.map(i +)).flatten
e1.map(i => s2.map(i +)).flatten
e1.map(i => e2.map(i +)).flatten

s1.filter(1 ==)
s2.filter(1 ==)
e1.filter(1 ==)

s1.filterNot(2 ==)
s2.filterNot(2 ==)
e1.filterNot(2 ==)

s1.doOnSuccess(println)
e1.doOnSuccess(println)

s1.doOnError(println)
e1.doOnError(println)

s1.eventually(() => println("hello"))
e1.eventually(() => println("world"))
