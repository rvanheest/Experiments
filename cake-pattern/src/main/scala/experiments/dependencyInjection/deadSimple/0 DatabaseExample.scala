package experiments.dependencyInjection.deadSimple

import java.sql.{Connection, DriverManager}

object Hardcoded {
	// function that updates password of user in database
	// everything hardcoded, not very reusable
	def setUserPassword(id: String, password: String): Unit = {
		Class.forName("org.sqlite.JDBC")
		val c = DriverManager.getConnection("jdbc:sqlite::memory:")
		val statement = c.prepareStatement("UPDATE users SET pwd = ? WHERE id = ?;")
		statement.setString(1, password)
		statement.setString(2, id)
		statement.executeUpdate()
		c.commit()
		c.close()
	}
}

object WithGlobalFactory {
	// one solution is to use a global ConnectionFactory
	object ConnectionFactory {
		def getConnection: Connection = {
			Class.forName("org.sqlite.JDBC")
			DriverManager.getConnection("jdbc:sqlite::memory:")
		}
	}
	def setUserPassword(id: String, password: String): Unit = {
		val c = ConnectionFactory.getConnection
		val statement = c.prepareStatement("UPDATE users SET pwd = ? WHERE id = ?;")
		statement.setString(1, password)
		statement.setString(2, id)
		statement.executeUpdate()
		statement.close()
		// am I responsable for closing the connection or not???
	}

	/*
	  Bad idea because:
	  1. hidden dependency
	  2. requires magic initialization step
	  3. testing
	 */
}

object InversionOfControl {
	// better solution: inversion of control - taking the connection as an argument of the function
	def setUserPassword(id: String, password: String, c: Connection): Unit = {
		val statement = c.prepareStatement("UPDATE users SET pwd = ? WHERE id = ?;")
		statement.setString(1, password)
		statement.setString(2, id)
		statement.executeUpdate()
		statement.close()
	}
	// problem: now you need to pass the connection through all your code to reach this function
}

object Currying {
	// currying the Connection is the real solution here
	def setUserPassword(id: String, password: String): Connection => Unit =
		c => {
			val statement = c.prepareStatement("UPDATE users SET pwd = ? WHERE id = ?;")
			statement.setString(1, password)
			statement.setString(2, id)
			statement.executeUpdate()
			statement.close()
		}
}

object WithDatatype {
	// curry function in data type for convenience
	// we can also define map and flatMap on it
	case class DB[T](f: Connection => T) {
		def apply(c: Connection): T = f(c)

		def map[S](g: T => S): DB[S] = {
			DB(g compose f)
		}

		def flatMap[S](g: T => DB[S]): DB[S] = {
			DB(c => g(f(c))(c))
		}
	}
	object DB {
		def apply[T](t: T): DB[T] = DB(_ => t)
	}

	def setUserPassword(id: String, password: String): DB[Unit] = {
		DB(c => {
			val statement = c.prepareStatement("UPDATE users SET pwd = ? WHERE id = ?;")
			statement.setString(1, password)
			statement.setString(2, id)
			statement.executeUpdate()
			statement.close()
		})
	}

	def getUserPassword(id: String): DB[String] = {
		DB(c => {
			val statement = c.prepareStatement("SELECT pwd FROM users WHERE id = ?;")
			statement.setString(1, id)
			val res = statement.executeQuery()
			res.next() // we assume the user is in there, so is the password
			val result = res.getString("pwd")
			res.close()

			result
		})
	}

	// now we can compose database actions in a monadic style
	// we're not talking about database connections at all, that is wrapped and threaded around in DB
	def changePassword(id: String, oldPassword: String, newPassword: String): DB[Boolean] = {
		for {
			password <- getUserPassword(id)
			eq <- if (password == oldPassword) setUserPassword(id, newPassword).map(_ => true)
						else DB(false)
		} yield eq
	}

	// then define an abstract class and a concrete implementation for providing the connection
	abstract class ConnectionProvider {
		def apply[T](db: DB[T]): T
	}
	def mkProvider(driver: String, url: String) = new ConnectionProvider {
		def apply[T](db: DB[T]): T = {
			Class.forName(driver)
			val connection = DriverManager.getConnection(url)

			try {
				db(connection)
			}
			finally {
				connection.close()
			}
		}
	}

	mkProvider("a", "b")(changePassword("c", "d", "e"))
}
