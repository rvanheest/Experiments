package experiments.cakePattern.realWorldScalaDependencyInjection

object first extends App {
	case class User(username: String, password: String)

	trait UserRepositoryComponent {
		val userRepository = new UserRepository

		class UserRepository {
			def authenticate(username: String, password: String): User = {
				val user = new User(username, password)
				println(s"authenticate user: $user")
				user
			}
			def create(user: User) = println(s"creating user: $user")
			def delete(user: User) = println(s"deleting user: $user")
		}
	}

	trait UserServiceComponent {
		this: UserRepositoryComponent =>

		val userService = new UserService

		class UserService {
			def authenticate(username: String, password: String): User = {
				userRepository.authenticate(username, password)
			}
			def create(username: String, password: String) = {
				userRepository.create(new User(username, password))
			}
			def delete(user: User) = {
				userRepository.delete(user)
			}
		}
	}

	// dependencies are statically typed
	// everything is immutable (using vals)
	object ComponentRegistry extends UserServiceComponent with UserRepositoryComponent

	val userService = ComponentRegistry.userService
	val user = userService.authenticate("test", "test")
	println(user)

	// although it seems nice, it really isn't:
	// there is a high coupling between the service implementation and its creation
	// the wiring is scattered all over our code base
}

object second extends App {
	case class User(username: String, password: String)

	// change the service vals to be abstract member fields
	trait UserRepositoryComponent {
		val userRepository: UserRepository

		class UserRepository {
			def authenticate(username: String, password: String): User = {
				val user = new User(username, password)
				println(s"authenticate user: $user")
				user
			}
			def create(user: User) = println(s"creating user: $user")
			def delete(user: User) = println(s"deleting user: $user")
		}
	}

	trait UserServiceComponent {
		this: UserRepositoryComponent =>

		val userService: UserService

		class UserService {
			def authenticate(username: String, password: String): User = {
				userRepository.authenticate(username, password)
			}
			def create(username: String, password: String) = {
				userRepository.create(new User(username, password))
			}
			def delete(user: User) = {
				userRepository.delete(user)
			}
		}
	}

	object ComponentRegistry extends UserServiceComponent
																	 with UserRepositoryComponent {
		val userRepository = new UserRepository
		val userService = new UserService
	}

	val userService = ComponentRegistry.userService
	val user = userService.authenticate("test", "test")
	println(user)

	// now component instantiation and wiring are abstracted away in a config object
	// this is also good for testing

	trait TestEnvironment extends UserServiceComponent
																with UserRepositoryComponent { // with Mocking trait, but I don't have that one here ;-)
	val userRepository = ??? // mock[UserRepository]
	val userService = ??? // mock[UserService]
	}

	class UserServiceSuite extends /*TestingFrameworkClass with*/ TestEnvironment {
		/*
			"authenticateUser" should "<text here>" in {
				// do the mocking and testing in here
			}
		 */
	}
}
