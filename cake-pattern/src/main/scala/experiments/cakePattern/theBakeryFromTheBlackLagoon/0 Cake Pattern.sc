case class User()

trait UserModule {
	def loadUser(id: Long): User
}

trait TweetModule {
	def post(userId: Long, body: String)
}

trait MySQLUserModule extends UserModule {
	def loadUser(id: Long): User = ???
}

trait TwitterModule extends TweetModule with UserModule {
	def post(userId: Long, body: String) = ???
}

val universe: UserModule with TweetModule = new MySQLUserModule with TwitterModule
