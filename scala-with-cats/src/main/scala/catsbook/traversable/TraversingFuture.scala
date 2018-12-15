package catsbook.traversable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

object TraversingFuture extends App {

  val hostNames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com",
  )

  def getUptime(hostname: String): Future[Int] = {
    Future(hostname.length * 60) // calculation just for demo
  }

  val allUptimes1 = hostNames.foldLeft(Future(List.empty[Int]))((accum, host) => {
    val uptime = getUptime(host)
    for {
      accum <- accum
      uptime <- uptime
    } yield accum :+ uptime
  })
  println(Await.result(allUptimes1, 1 second)) // List(1020, 960, 840)

  val allUptimes2 = Future.traverse(hostNames)(getUptime)
  println(Await.result(allUptimes2, 1 second)) // List(1020, 960, 840)

  println(Await.result(Future.sequence[Int, List](hostNames.map(getUptime)), 1 second)) // List(1020, 960, 840)
}
