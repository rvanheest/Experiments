package experiments.cakePattern.inDepth

import scala.collection.mutable.ListBuffer

case class User(id: Long, firstName: String, lastName: String)

trait UserRepositoryComponent {
	def userLocator: UserLocator
	def userUpdater: UserUpdater

	trait UserLocator {
		def findAll: List[User]
	}
	trait UserUpdater {
		def save(user: User): Unit
	}
}

trait UserRepositoryInMemComponent extends UserRepositoryComponent {
	val list: ListBuffer[User]
	def userLocator = new UserLocatorInMem(list)
	def userUpdater = new UserUpdaterInMem(list)

	class UserLocatorInMem(val list: ListBuffer[User]) extends UserLocator {
		def findAll: List[User] = list.toList
	}
	class UserUpdaterInMem(val list: ListBuffer[User]) extends UserUpdater {
		def save(user: User): Unit = list += user
	}
}

trait UserServiceComponent {
	def userService: UserService

	trait UserService {
		def findAll: List[User]
		def save(user: User): Unit
	}
}

trait DefaultUserServiceComponent extends UserServiceComponent {
	this: UserRepositoryComponent =>

	def userService = new DefaultUserService

	class DefaultUserService extends UserService {
		def findAll: List[User] = userLocator.findAll
		def save(user: User): Unit = userUpdater.save(user)
	}
}

object ApplicationLive {
	val userServiceComponent = new DefaultUserServiceComponent with UserRepositoryInMemComponent {
		val list = new ListBuffer[User]
	}
	val userService = userServiceComponent.userService
}

object Main extends App {
	val userService = ApplicationLive.userService

	println(userService.findAll)

	userService.save(User(1L, "first1", "last1"))
	userService.save(User(2L, "first2", "last2"))
	userService.save(User(3L, "first3", "last3"))
	userService.save(User(4L, "first4", "last4"))

	println(userService.findAll)
}
