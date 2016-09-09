import monadics.ScalaMonads.{FunctionExtension, functionIsMonad}

val x: Int => String = i => i.toString
val y: Int => String = x.flatMap(s => i => s"$s$i")
val z: Int => String = x.map(s => s"abc$s")

def a(i: Int) = i.toString
def b(i: Int) = (a _).flatMap(s => i => s"$s$i")(i)
def c(i: Int) = (a _).map(s => s"abc$s")(i)

x.apply(5)
y.apply(4)
z.apply(3)

a(5)
b(4)
c(3)
