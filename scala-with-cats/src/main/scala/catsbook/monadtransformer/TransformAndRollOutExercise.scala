package catsbook.monadtransformer

import cats.data.EitherT
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

object TransformAndRollOutExercise extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10,
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot)
      .map(avg => EitherT.right[String](Future(avg)))
      .getOrElse { EitherT.left[Int](Future(s"$autobot unreachable")) }
  }

  def canSpecialMove(autobot1: String, autobot2: String): Response[Boolean] = {
    for {
      level1 <- getPowerLevel(autobot1)
      level2 <- getPowerLevel(autobot2)
    } yield level1 + level2 > 15
  }

  def tacticalReport(autobot1: String, autobot2: String): String = {
    Await.result(canSpecialMove(autobot1, autobot2).value, 10 seconds)
      .fold("Comm error: " +, {
        case true => s"$autobot1 and $autobot2 are ready to roll out!"
        case false => s"$autobot1 and $autobot2 need to recharge."
      })
  }

  println(tacticalReport("Jazz", "Bumblebee")) // Jazz and Bumblebee need to recharge.
  println(tacticalReport("Bumblebee", "Hot Rod")) // Bumblebee and Hot Rod are ready to roll out!
  println(tacticalReport("Jazz", "Ironhide")) // Comms error: Ironhide unreachable
}
