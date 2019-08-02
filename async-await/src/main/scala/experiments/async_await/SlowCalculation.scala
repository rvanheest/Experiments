package experiments.async_await

import scala.async.Async.async
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

trait SlowCalculation {

  def slowCalcFuture(name: String): Future[Int] = async {
    println(Thread.currentThread().getName + " - " + name)

    Thread.sleep(5000)

    42
  }
}
