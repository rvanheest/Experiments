import java.nio.ByteBuffer
import javafx.scene.Node

import scala.Specializable.Group

// a trait is a module!
// allow for explicit typechecked dependencies
// give complete encapsulation
//   - not only method implementations
//   - also type implementations
// OOP: the way it was meant

/*
	Stage 1:
	1. Split into top-level modules (traits)
	   (one per file)
	2. extends and with instead of import
	3. use bare functions as liberally as desired
	4. inner objects to break monotony
 */

// Example
trait UserModule1 {
	def login(name: String, pass: String) = {
		// do stuff
	}

	def save(user: User): Unit = {
		// do more stuff
	}

	case class User(/* ... */)
}
trait MessageModule1a extends UserModule1 {
	def render(msg: Message): Group[Node] = {
		// yay, anti-xml user!
		???
	}

	def save(msg: Message): Unit = {
		// stuff
	}

	case class Message(author: User, body: String)
}

// name collision on save.
// put everything about message in inner object
trait MessageModule1b extends UserModule {
	object message {
		def render(msg: Message): Group[Node] = {
			// yay, anti-xml user!
			???
		}

		def save(msg: Message): Unit = {
			// stuff
		}
	}

	case class Message(author: User, body: String)
}

/*
  Stage 2:
  1. Refactor functions to abstract methods
  2. Refactor classes to abstract types (virtual classes)
  3. Use bounds for successive refinement
  4. Lifecycle management (abstract override)
 */
// this is good for testing, mocking gets easier as modules are traits
// never put side effects inside trait constructors, instead use stackable trait patterns

// Example
trait UserModule2a {
	// make methods abstract
	def login(name: String, pass: String): Option[User]

	def save(user: User): Unit

	case class User(/* ... */)
}
// several implementations of UserModule2a
// ...
// MessageModule2 depends on the abstract UserModule2a, not on some implementation
// hide the details of how to login
trait MessageModule2 extends UserModule2a {
	// ...
}

// maybe we want save as a function in User
// for that we make the concept of a User abstract
trait UserModule2b {

	type User <: UserLike

	trait UserLike { this: User =>
		def id: String
		def name: String

		def save(): Unit
	}

	def User(id: String, name: String): User
}
trait MongoUserModule2a extends UserModule2b {

	class User(override val id: String, override val name: String) extends UserLike {
		def save(): Unit = ???
	}

	def User(id: String, name: String): User = new User(id, name)
}

// lifecycle module
// startup and shutdown of stuff in the app
// every other module that needs a startup or shutdown routine implements this
// multiple modules with startup/shutdown => 'abstract override' (stackable traits)
trait Lifecycle {
	def startup(): Unit
	def shutdown(): Unit
}
trait MongoUserModule2b extends UserModule2b with Lifecycle {

	// 'abstract override' allows you to access the 'super pointer'
	// super calls allow for composition of Lifecycle modules
	// order of extends is important here! no dependencies between startup and teardown routines in different modules
	abstract override def startup(): Unit = {
		super.startup()
		// init mongo thingy
	}

	abstract override def shutdown(): Unit = {
		// kill mongo thing
		super.shutdown()
	}

	// the rest of the stuff from MongoUserModule2a
}

/*
  Stage 3:
  1. Composition over inheritance (sometimes)
  2. Modules within modules (independent lifecycles)
 */
// composition when shared state between modules or instances of a module

// Example
trait SystemModule3a {
	// userModules lifecycle is separate from SystemModule's lifecycle
	// SystemModule is NOT a UserModule, so inheritance makes no sense
	val userModule: UserModule2b

	def doStuff() = {
//		userModule.makeUsers()
//		userModule.encourageMemes()
		// ???
//		userModule.profit()
	}
}

trait StorageModule {
	def store(id: Long, data: ByteBuffer)
	def retrieve(id: Long): ByteBuffer
}
trait SystemModule3b extends StorageModule {
	def doStuff(userModule: UserModule2b) = {
		// ...
	}
}
// we want the userModule to have access to the storageModule (user stores data)
// this is a problem when you say that they have separate lifecycles

trait UserModuleModule extends StorageModule {
	trait UserModule {
//		type User <: UserLike
//		...
	}
}
trait SystemModule3c extends StorageModule with UserModuleModule {
	def doStuff(userModule: UserModule) = {
		// ...
	}
}

