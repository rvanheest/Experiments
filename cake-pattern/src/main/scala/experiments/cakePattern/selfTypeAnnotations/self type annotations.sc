val x = 4 // just to get rid of warnings in IntelliJ

// B is requiring A (annotating)
trait A
trait B { this: A => }

// D is being a C (inheritance)
trait C
trait D extends C

// inheritance leaks functionality into interfaces that you don't intend
trait E {
	def foo = "foo"
}
trait F { this: E =>
	def foobar = foo + "bar"
}
// the following doesn't work:
//trait G { this: F =>
//	def fooNope = foo + "Nope"
//}

trait H {
	def foo = "foo"
}
trait I extends H {
	def foobar = foo + "bar"
}
// the following does work due to the inheritance:
trait J extends I {
	def fooYep = foo + "Yep"
}

/*
  this is the difference between
    * F is requiring E - no functionality leaks
    * I is being a H - functionality leaks
 */

// example
object inheritance {
	trait Database {
		def query(/* parameters */): Any = ???
	}
	trait UserDB extends Database {
		def getUserData(/* parameters */): Any = ???
	}
	trait EmailService extends UserDB {
		// Has access to all the Database methods
		// when it only should just be able to talk to the UserDb abstraction
	}

	val emailService = new EmailService {}
}

object selfTypeAnnotation {
	trait Database {
		def query(/* parameters */): Any
	}
	trait MongoDatabase extends Database {
		override def query() = ???
	}
	trait UserDB { this: Database =>
		def getUserData(/* parameters */): Any = ???
	}
	trait EmailService { this: UserDB =>

		// Can only access UserDb methods, cannot access Database methods
		val userData = getUserData()
	}
	object EmailService {
		def apply(): EmailService = new EmailService() with UserDB with MongoDatabase
	}

	val emailService = EmailService()
	println(emailService.userData)
}
