package catsbook.traversable

import cats.Applicative
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.apply._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps

object TraverseWithCats extends App {

  val hostNames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com",
  )

  def getUptime(hostname: String): Future[Int] = {
    Future(hostname.length * 60) // calculation just for demo
  }

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F])((accum, item) => (accum, func(item)).mapN(_ :+ _))
  }

  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = listTraverse(list)(identity)

  val totalUptime = listTraverse(hostNames)(getUptime)
  println(Await.result(totalUptime, 1 second)) // List(1020, 960, 840)
}
