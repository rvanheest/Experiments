import monadics.ScalaMonads.{FunctionExtension, functionIsMonadPlus}

val x: Int => String = i => i.toString
val y: Int => String = x.flatMap(s => i => s"$s$i")
val z: Int => String = x.map(s => s"abc$s")

x.apply(5)
y.apply(4)
z.apply(3)
