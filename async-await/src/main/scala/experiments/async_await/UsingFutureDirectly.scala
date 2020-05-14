package experiments.async_await

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

object UsingFutureDirectly extends App with SlowCalculation {

  val future1: Future[Int] = slowCalcFuture("left")
  val future2: Future[Int] = slowCalcFuture("right")

  def combined: Future[Int] = for {
    r1 <- future1
    r2 <- future2
  } yield r1 + r2

  println(Await.result(combined, 10 seconds))
}
