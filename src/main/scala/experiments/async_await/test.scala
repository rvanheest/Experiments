package experiments.async_await

import experiments.async_await.parallelInListWithFuture.slowCalcFuture

import scala.async.Async.{ async, await }
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

object simple extends App {
  val future: Future[Int] = async {
    val f1 = async(true)
    val f2 = async(42)
    if (await(f1)) await(f2)
    else 0
  }

  println(Await.result(future, 5 seconds))
}

trait SlowCalculation {
  def slowCalcFuture(name: String): Future[Int] = async { println(Thread.currentThread().getName + " - " + name); Thread.sleep(5000); 42 }
}

object nonParallel extends App with SlowCalculation {
  def combined: Future[Int] = async {
    println(Thread.currentThread().getName + " - combined")
    await(slowCalcFuture("left")) + await(slowCalcFuture("right"))
  }

  lazy val x: Int = Await.result(combined, 10 seconds)
  lazy val y: Int = Await.result(combined, 9 seconds) // this will fail because the max timeout is smaller than the computation time

  println(x)
  println(y)
}

object parallel extends App with SlowCalculation {
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

object usingFutureDirectly extends App with SlowCalculation {
  val future1: Future[Int] = slowCalcFuture("left")
  val future2: Future[Int] = slowCalcFuture("right")

  def combined: Future[Int] = for {
    r1 <- future1
    r2 <- future2
  } yield r1 + r2

  println(Await.result(combined, 10 seconds))
}

object parallelInListWitAsyncAwait extends App with SlowCalculation {
  def futures: immutable.IndexedSeq[Future[Int]] = (1 to 5).map(s"job " +).map(x => async(await(slowCalcFuture(x))))
  def combined: Future[Int] = Future.sequence(futures).map(_.sum)

  lazy val x: Int = Await.result(combined, 25 seconds)
  lazy val y: Int = Await.result(combined, 5 seconds)

  println(x)
  println(y)
}

object parallelInListWithFuture extends App with SlowCalculation {
  def futures: immutable.IndexedSeq[Future[Int]] = (1 to 5).map(s"job " +).map(slowCalcFuture)
  def combined: Future[Int] = Future.sequence(futures).map(_.sum)

  lazy val x: Int = Await.result(combined, 25 seconds)
  lazy val y: Int = Await.result(combined, 6 seconds)

  println(x)
  println(y)
}
