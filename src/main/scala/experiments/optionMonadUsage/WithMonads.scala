package experiments.optionMonadUsage

object WithMonads {

	case class Foo2(bar: Option[Bar2])
	case class Bar2(baz: Option[Baz2])
	case class Baz2() {
		def compute: Int = 1
	}

	def computeBaz(baz: Baz2): Int = baz.compute
	def computeBar(bar: Bar2): Option[Int] = bar.baz.map(computeBaz)
	def computeFoo(foo: Foo2): Option[Int] = foo.bar.flatMap(computeBar)
	def compute(maybeFoo: Option[Foo2]): Option[Int] = maybeFoo.flatMap(computeFoo)

	def compute2(maybeFoo: Option[Foo2]): Option[Int] = {
		for {
			foo <- maybeFoo
			bar <- foo.bar
			baz <- bar.baz
		} yield baz.compute
	}

	def compute3(foo: Foo2): Option[Int] = {
		for {
			bar <- foo.bar
			baz <- bar.baz
		} yield baz.compute
	}
}
