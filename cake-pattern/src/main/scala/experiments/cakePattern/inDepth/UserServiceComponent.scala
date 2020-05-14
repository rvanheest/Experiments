package experiments.cakePattern.inDepth

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
