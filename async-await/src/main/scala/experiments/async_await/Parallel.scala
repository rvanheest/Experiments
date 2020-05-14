package experiments.async_await

import scala.async.Async.{ async, await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

object Parallel extends App with SlowCalculation {

  def combined: Future[Int] = async {
    println(Thread.currentThread().getName + " - combined")
    val left = slowCalcFuture("left")
    val right = slowCalcFuture("right")
    await(left) + await(right)
  }

  lazy val x: Int = Await.result(combined, 10 seconds)
  lazy val y: Int = Await.result(combined, 9 seconds) // this will not fail because `left` and `right` are run in parallel

  println(x)
  println(y)
}
