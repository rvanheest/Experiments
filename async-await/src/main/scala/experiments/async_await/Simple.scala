package experiments.async_await

import scala.async.Async.{ async, await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

object Simple extends App {
  val future: Future[Int] = async {
    val f1 = async(true)
    val f2 = async(42)
    if (await(f1)) await(f2)
    else 0
  }

  println(Await.result(future, 5 seconds))
}
