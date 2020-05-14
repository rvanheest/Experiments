package experiments.async_await

import scala.async.Async.{ async, await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

object NonParallel extends App with SlowCalculation {

  def combined: Future[Int] = async {
    println(Thread.currentThread().getName + " - combined")
    await(slowCalcFuture("left")) + await(slowCalcFuture("right"))
  }

  lazy val x: Int = Await.result(combined, 10 seconds)
  lazy val y: Int = Await.result(combined, 9 seconds) // this will fail because the max timeout is smaller than the computation time

  println(x)
  println(y)
}
