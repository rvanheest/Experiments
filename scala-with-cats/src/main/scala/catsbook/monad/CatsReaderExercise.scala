package catsbook.monad

import cats.data.Reader
import cats.syntax.applicative._

object CatsReaderExercise extends App {

  type UserId = Int
  type Username = String
  type Password = String

  case class Db(usernames: Map[UserId, Username],
                passwords: Map[Username, Password])

  type DbReader[A] = Reader[Db, A]
  val DbReader = Reader

  def findUsername(userId: UserId): DbReader[Option[Username]] = DbReader(_.usernames get userId)

  def checkPassword(username: Username, password: Password): DbReader[Boolean] = {
    DbReader(_.passwords get username contains password)
  }

  def checkLogin(userId: UserId, password: Password): DbReader[Boolean] = {
    for {
      username <- findUsername(userId)
      validPassword <- username.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
    } yield validPassword
  }

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo",
  )
  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret",
  )
  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db)) // true
  println(checkLogin(2, "invalid").run(db)) // false
  println(checkLogin(4, "davinci").run(db)) // false
}
