package experiments.cakePattern.theBakeryFromTheBlackLagoon

import java.nio.ByteBuffer

/*
  a trait is a module!
  allow for explicit typechecked dependencies
  give complete encapsulation
    - not only method implementations
    - also type implementations
  OOP: the way it was meant
 */

/*
	Stage 1:
	1. Split into top-level modules (traits)
	   (one per file)
	2. extends and with instead of import
	3. use bare functions as liberally as desired
	4. inner objects to break monotony
 */
object Stage1 {

	// Example
	object Phase0 {
		trait UserModule {
			def login(name: String, pass: String) = {
				// do stuff
			}

			def save(user: User): Unit = {
				// do stuff
			}

			case class User(/* parameters */)
		}
		trait MessageModule extends UserModule {
			def render(msg: Message): Any = {
				???
			}

			def save(msg: Message): Unit = {
				// stuff
			}

			case class Message(author: User, body: String)
		}
	}

	// name collision on the save method (method overloading).
	// put everything about message in inner object
	object Phase1 {
		trait MessageModule extends Phase0.UserModule {
			object message {
				def render(msg: Message): Any = {
					???
				}

				def save(msg: Message): Unit = {
					// stuff
				}
			}

			case class Message(author: User, body: String)
		}
	}
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
object Stage2 {

	// Example
	object Phase0 {
		trait UserModule {
			// make methods abstract
			def login(name: String, pass: String): Option[User]

			def save(user: User): Unit

			case class User(/* ... */)
		}

		// several implementations of UserModule
		// ...
		// MessageModule depends on the abstract UserModule, not on some implementation
		// hide the details of how to login
		trait MessageModule extends UserModule {
			object message {
				def render(msg: Message): Any = {
					???
				}

				def save(msg: Message): Unit = {
					// stuff
				}
			}

			case class Message(author: User, body: String)
		}
	}

	// maybe we want `save` as a function in User
	// for that we make the concept of a User abstract
	object Phase1 {
		trait UserModule {

			type User <: UserLike

			trait UserLike { this: User =>
				def id: String
				def name: String

				def save(): Unit
			}

			def User(id: String, name: String): User
		}
		trait MongoUserModule extends UserModule {

			class User(override val id: String, override val name: String) extends UserLike {
				def save(): Unit = ???
			}

			def User(id: String, name: String): User = new User(id, name)
		}
	}

	// lifecycle module
	// startup and shutdown of stuff in the app
	// every other module that needs a startup or shutdown routine implements this
	// multiple modules with startup/shutdown => 'abstract override' (stackable traits)
	object Phase2 {
		trait Lifecycle {
			def startup(): Unit
			def shutdown(): Unit
		}
		trait MongoUserModule extends Phase1.UserModule with Lifecycle {

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

			class User(override val id: String, override val name: String) extends UserLike {
				def save(): Unit = ???
			}

			def User(id: String, name: String): User = new User(id, name)
		}
	}
}

object Stage3 {
	// Example
	object Phase0 {
		trait SystemModule {
			// userModules lifecycle is separate from SystemModule's lifecycle
			// SystemModule is NOT a UserModule, so inheritance makes no sense
			val userModule: Stage2.Phase1.UserModule

			def doStuff() = {
				// userModule.makeUsers()
				// userModule.encourageMemes()
				// ???
				// userModule.profit()
			}
		}
	}

	object Phase1 {
		trait StorageModule {
			def store(id: Long, data: ByteBuffer)
			def retrieve(id: Long): ByteBuffer
		}
		trait SystemModule extends StorageModule {
			def doStuff(userModule: Stage2.Phase1.UserModule) = {
				// ...
			}
		}
	}

	// we want the userModule to have access to the storageModule (user stores data)
	// this is a problem when you say that they have separate lifecycles
	object Phase2 {
		trait UserModuleModule extends Phase1.StorageModule {
			trait UserModule {

				type User <: UserLike

				trait UserLike { this: User =>
					def id: String
					def name: String

					def save(): Unit
				}

				def User(id: String, name: String): User
			}
		}
		trait SystemModule extends Phase1.StorageModule with UserModuleModule {
			def doStuff(userModule: UserModule) = {
				// ...
			}
		}
	}
}
