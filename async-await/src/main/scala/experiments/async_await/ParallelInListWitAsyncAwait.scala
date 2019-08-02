package experiments.async_await

import scala.async.Async.{ async, await }
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

object ParallelInListWitAsyncAwait extends App with SlowCalculation {

  def futures: immutable.IndexedSeq[Future[Int]] = (1 to 5).map(s"job " +).map(x => async(await(slowCalcFuture(x))))

  def combined: Future[Int] = Future.sequence(futures).map(_.sum)

  lazy val x: Int = Await.result(combined, 25 seconds)
  lazy val y: Int = Await.result(combined, 5 seconds)

  println(x)
  println(y)
}
