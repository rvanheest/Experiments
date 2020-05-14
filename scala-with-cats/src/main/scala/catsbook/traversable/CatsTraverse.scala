package catsbook.traversable

import cats.Traverse
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

object CatsTraverse extends App {

  val hostNames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com",
  )

  def getUptime(hostname: String): Future[Int] = {
    Future(hostname.length * 60) // calculation just for demo
  }

  val totalUptime = Traverse[List].traverse(hostNames)(getUptime)
  println(Await.result(totalUptime, 1 second)) // List(1080, 960, 840)

  val numbers = List(Future(1), Future(2), Future(3))
  val numbers2 = Traverse[List].sequence(numbers)
  println(Await.result(numbers2, 1 second)) // List(1, 2, 3)

  // use syntax
  println(Await.result(hostNames.traverse(getUptime), 1 second)) // List(1080, 960, 840)
  println(Await.result(numbers.sequence[Future, Int], 1 second)) // List(1, 2, 3)
}
