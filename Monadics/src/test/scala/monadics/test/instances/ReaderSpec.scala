package monadics.test.instances

class ReaderSpec extends InstanceSpec {

	property("reader 'run'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String) =>
			Reader(f).run(x) shouldBe f(x)
		}
	}

	property("reader 'map'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: String => Long) =>
			Reader(f).map(g).run(x) shouldBe (g compose f)(x)
		}
	}

	property("reader 'as'") {
		import monadics.instances.Reader

		forAll { (x: Int, y: Long, f: Int => String) =>
			Reader(f).as(y).run(x) shouldBe y
		}
	}

	property("reader 'void'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String) =>
			Reader(f).void.run(x) shouldBe ()
		}
	}

	property("reader 'zipWith'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: String => Long) =>
			Reader(f).zipWith(g).run(x) shouldBe (f(x), (g compose f)(x))
		}
	}

	property("reader '<*>'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: Int => String => Long) =>
			(Reader(g) <*> Reader(f)).run(x) shouldBe g(x)(f(x))
		}
	}

	property("reader '*>") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: Int => Long) =>
			(Reader(f) *> Reader(g)).run(x) shouldBe g(x)
		}
	}

	property("reader '<*'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: Int => Long) =>
			(Reader(f) <* Reader(g)).run(x) shouldBe f(x)
		}
	}

	property("reader '<**>'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: Int => String => Long) =>
			(Reader(f) <**> Reader(g)).run(x) shouldBe g(x)(f(x))
		}
	}

	property("reader 'flatMap'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: String => Reader[Int, Long]) =>
			Reader(f).flatMap(g).run(x) shouldBe g(f(x)).run(x)
		}
	}

	property("reader 'andThen") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: Int => Long) =>
			Reader(f).andThen(Reader(g)).run(x) shouldBe g(x)
		}
	}

	property("reader 'thenAnd'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: Int => Long) =>
			Reader(f).thenAnd(Reader(g)).run(x) shouldBe f(x)
		}
	}

	property("reader 'flatten'") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => Reader[Int, String]) =>
			Reader(f).flatten.run(x) shouldBe f(x).run(x)
		}
	}

	property("reader as function") {
		import monadics.instances.Reader

		forAll { (x: Int, f: Int => String, g: String => Long) =>
			(Reader(g) compose Reader(f))(x) shouldBe (g compose f)(x)
		}
	}
}
