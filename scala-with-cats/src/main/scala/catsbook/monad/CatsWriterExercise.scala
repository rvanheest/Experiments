package catsbook.monad

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

object CatsWriterExercise extends App {

  def slowly[A](body: => A): A = {
    try {
      body
    }
    finally {
      Thread.sleep(1000)
    }
  }

  def factorial(n: Int): Int = {
    val ans = slowly {
      if (n == 0) 1
      else n * factorial(n - 1)
    }
    println(s"fact $n $ans")
    ans
  }

//  println(factorial(5))

//  Await.result(Future.sequence(Vector(
//    Future(factorial(3)),
//    Future(factorial(3)),
//  )), 5 seconds)

  type Logged[A] = Writer[Vector[String], A]

  def factorialWriter(n: Int): Writer[Vector[String], Int] = {
    for {
      ans <- slowly {
        if (n == 0) 1.pure[Logged]
        else factorialWriter(n - 1).map(_ * n)
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

//  val (log, result) = factorialWriter(5).run
//  println(log)
//  println(result)

  val Vector((logA, resultA), (logB, resultB)) = Await.result(Future.sequence(Vector(
    Future(factorialWriter(3).run),
    Future(factorialWriter(5).run)
  )), 10 seconds)
  println(logA)
  println(resultA)
  println(logB)
  println(resultB)
}
