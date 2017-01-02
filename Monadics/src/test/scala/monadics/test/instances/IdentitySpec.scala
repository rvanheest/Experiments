package monadics.test.instances

import scala.language.postfixOps

class IdentitySpec extends InstanceSpec {

	private def continuously[A](x: => A): List[A] = Stream.continually(x).toList

	property("identity 'run'") {
		import monadics.instances.Identity

		forAll { (x: Int) =>
			Identity(x).run shouldBe x
		}
	}

	property("identity 'map'") {
		import monadics.instances.Identity

		forAll { (x: Int, f: Int => String) =>
			Identity(x).map(f).run shouldBe f(x)
		}
	}

	property("identity 'as'") {
		import monadics.instances.Identity

		forAll { (x: Int, y: String) =>
			Identity(x).as(y).run shouldBe y
		}
	}

	property("identity 'void'") {
		import monadics.instances.Identity

		forAll { (x: Int) =>
			Identity(x).void.run shouldBe ()
		}
	}

	property("identity 'zipWith'") {
		import monadics.instances.Identity

		forAll { (x: Int, f: Int => String) =>
			Identity(x).zipWith(f).run shouldBe (x, f(x))
		}
	}

	property("identity '<*>'") {
		import monadics.instances.Identity

		forAll { (x: Int, f: Int => String) =>
			(Identity(f) <*> Identity(x)).run shouldBe f(x)
		}
	}

	property("identity '*>'") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			(Identity(x) *> Identity(y)).run shouldBe y
		}
	}

	property("identity '<*'") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			(Identity(x) <* Identity(y)).run shouldBe x
		}
	}

	property("identity '<**>'") {
		import monadics.instances.Identity

		forAll { (x: Int, f: Int => String) =>
			(Identity(x) <**> Identity(f)).run shouldBe f(x)
		}
	}

	property("identity 'flatMap'") {
		import monadics.instances.Identity

		forAll { (x: Int, f: Int => Identity[String]) =>
			Identity(x).flatMap(f).run shouldBe f(x).run
		}
	}

	property("identity 'andThen'") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			Identity(x).andThen(Identity(y)).run shouldBe y
		}
	}

	property("identity 'thenAnd'") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			Identity(x).thenAnd(Identity(y)).run shouldBe x
		}
	}

	property("identity 'flatten'") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			Identity(Identity(x)).flatten.run shouldBe x
		}
	}

	property("identity 'map' lazyness") {
		import monadics.instances.Identity

		forAll { (x: Int) =>
			Identity(continuously(x)).map(_.map(1 +))
		}
	}

	property("identity '<*>' lazyness") {
		import monadics.instances.Identity

		forAll { (x: Int, f: List[Int] => String) =>
			Identity(f) <*> Identity(continuously(x))
		}
	}

	property("identity '*>' lazyness") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			Identity(continuously(x)) *> Identity(y)
		}
	}

	property("identity '<*' lazyness") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			Identity(continuously(x)) <* Identity(y)
		}
	}

	property("identity '<**>' lazyness") {
		import monadics.instances.Identity

		forAll { (x: Int, f: List[Int] => String) =>
			Identity(continuously(x)) <**> Identity(f)
		}
	}

	property("identity 'flatMap' lazyness") {
		import monadics.instances.Identity

		forAll { (x: Int, f: List[Int] => Identity[String]) =>
			Identity(continuously(x)).flatMap(f)
		}
	}

	property("identity 'andThen' lazyness") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			Identity(continuously(x)).andThen(Identity(y))
		}
	}

	property("identity 'thenAnd' lazyness") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			Identity(continuously(x)).thenAnd(Identity(y))
		}
	}

	property("identity 'flatten' lazyness") {
		import monadics.instances.Identity

		forAll { (x: Int, y: Int) =>
			Identity(Identity(continuously(x))).flatten
		}
	}
}
