import experiments.tryMonad.Try

val s1 = Try(1)
val s2 = Try(2)
val e1 = Try.error(new IllegalArgumentException("foobar"))

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

// TODO continue testing here
