package catsbook.functor

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

object FunctorExamples extends App {

  println {
    List(1, 2, 3).map(n => n + 1) // [2, 3, 4]
  }
  println {
    List(1, 2, 3)
      .map(n => n + 1)
      .map(n => n * 2)
      .map(n => n + "!") // [4!, 6!, 8!]
  }

  println {
    Option(1).map(n => n + 1) // Some(2)
  }
  println {
    Option.empty[Int].map(n => n + 1) // None
  }

  println {
    Right[String, Int](1)
      .map(n => n + 1) // Right(2)
  }
  println {
    Left[String, Int]("hello")
      .map(n => n + 1) // Left("hello")
  }

  println {
    val future = Future(123)
      .map(n => n + 1)
      .map(n => n * 2)
      .map(n => n + "!")
    Await.result(future, 1 second)
  }

  val future1 = {
    val r = new Random(0L)
    val x = Future(r.nextInt)

    for {
      a <- x
      b <- x
    } yield (a, b)
  }

  val future2 = {
    val r = new Random(0L)

    for {
      a <- Future(r.nextInt)
      b <- Future(r.nextInt)
    } yield (a, b)
  }

  println(Await.result(future1, 1 second))
  println(Await.result(future2, 1 second))
}
