package experiments.cakePattern.inDepth

import scala.collection.mutable.ListBuffer

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
		def findAll: List[User] = {
			list.toList
		}
	}

	class UserUpdaterInMem(val list: ListBuffer[User]) extends UserUpdater {
		def save(user: User): Unit = {
			list += user
		}
	}
}
