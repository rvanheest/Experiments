import experiments.scala.optionMonadUsage.OldStyle.{Foo, Bar, Baz}
import experiments.scala.optionMonadUsage.{OldStyle, WithMonads}
import experiments.scala.optionMonadUsage.WithMonads.{Foo2, Bar2, Baz2}

val baz = Baz()
val bar = Bar(baz)
val foo = Foo(bar)
OldStyle.compute(foo)
OldStyle.compute2(foo)

val baz2 = Baz2()
val bar2 = Bar2(Option(baz2))
val foo2 = Foo2(Option(bar2))
WithMonads.compute(Option(foo2))
WithMonads.compute2(Option(foo2))
WithMonads.compute3(foo2)
