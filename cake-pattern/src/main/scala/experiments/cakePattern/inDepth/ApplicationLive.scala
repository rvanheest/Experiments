package experiments.cakePattern.inDepth

import scala.collection.mutable.ListBuffer

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
