package experiments.async_await

import scala.async.Async.{async, await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

object simple extends App {
  val future: Future[Int] = async {
    val f1 = async(true)
    val f2 = async(42)
    if (await(f1)) await(f2) else 0
  }

  println(Await.result(future, 5 seconds))
}

object nonParallel extends App {
  def slowCalcFuture(name: String): Future[Int] = async { println(Thread.currentThread().getName + " - " + name); Thread.sleep(5000); 42 }
  def combined: Future[Int] = async {
    println(Thread.currentThread().getName + " - combined")
    await(slowCalcFuture("left")) + await(slowCalcFuture("right"))
  }
  lazy val x: Int = Await.result(combined, 10 seconds)
  lazy val y: Int = Await.result(combined, 9 seconds) // this will fail because the max timeout is smaller than the computation time

  println(x)
  println(y)
}

object parallel extends App {
  def slowCalcFuture(name: String): Future[Int] = async { println(Thread.currentThread().getName + " - " + name); Thread.sleep(5000); 42 }
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

object usingFutureDirectly extends App {
  def slowCalcFuture(name: String): Future[Int] = async { println(Thread.currentThread().getName + " - " + name); Thread.sleep(5000); 42 }
  val future1: Future[Int] = slowCalcFuture("left")
  val future2: Future[Int] = slowCalcFuture("right")
  def combined: Future[Int] = for {
    r1 <- future1
    r2 <- future2
  } yield r1 + r2

  println(Await.result(combined, 10 seconds))
}
