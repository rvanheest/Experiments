package experiments.optionMonadUsage

object OldStyle {

	case class Foo(bar: Bar)
	case class Bar(baz: Baz)
	case class Baz() {
		def compute: Int = 1
	}

	def compute(foo: Foo): Integer = {
		if (foo == null)
			return null
		val bar = foo.bar
		if (bar == null)
			return null
		val baz = bar.baz
		if (baz == null)
			return null
		baz.compute
	}

	def compute2(foo: Foo): Integer = {
		if (foo != null) {
			val bar = foo.bar
			if (bar != null) {
				val baz = bar.baz
				if (baz != null) baz.compute
				else null
			}
			else null
		}
		else null
	}
}
