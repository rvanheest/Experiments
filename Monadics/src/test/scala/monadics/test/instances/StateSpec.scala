package monadics.test.instances

class StateSpec extends InstanceSpec {

	property("state 'run'") {
		import monadics.instances.State

		forAll { (x: Int, f: Int => (String, Int)) =>
			State(f).run(x) shouldBe f(x)
		}
	}

	property("state 'evaluate'") {
		import monadics.instances.State

		forAll { (x: Int, f: Int => (String, Int)) =>
			State(f).evaluate(x) shouldBe f(x)._1
		}
	}

	property("state 'execute'") {
		import monadics.instances.State

		forAll { (x: Int, f: Int => (String, Int)) =>
			State(f).execute(x) shouldBe f(x)._2
		}
	}

	property("state 'map'") {
		import monadics.instances.State

		forAll { (x: Int, f: Int => (String, Int), g: String => Long) =>
			val (l, i) = State(f).map(g).run(x)
			i shouldBe f(x)._2
			l shouldBe g(f(x)._1)
		}
	}

	property("state 'as'") {
		import monadics.instances.State

		forAll { (x: Int, y: Long, f: Int => (String, Int)) =>
			val (l, i) = State(f).as(y).run(x)
			i shouldBe f(x)._2
			l shouldBe y
		}
	}

	property("state 'void'") {
		import monadics.instances.State

		forAll { (x: Int, f: Int => (String, Int)) =>
			val (u, i) = State(f).void.run(x)
			i shouldBe f(x)._2
			u shouldBe ()
		}
	}

	property("state 'zipWith'") {
		import monadics.instances.State

		forAll { (x: Int, f: Int => (String, Int), g: String => Long) =>
			val ((s, l), i) = State(f).zipWith(g).run(x)
			(s, i) shouldBe f(x)
			l shouldBe g(f(x)._1)
		}
	}
}
