package experiments.taglessfinal.effect.cats

import cats.Monad
import experiments.taglessfinal.effect.cats.ConsoleOut._
import experiments.taglessfinal.effect.cats.Monad.MonadSyntax

import scala.language.{ higherKinds, postfixOps }
import scala.util.Try

object Game {

  def runApplication[F[_] : Monad : Random : Console]: F[Unit] = {
    for {
      _ <- println(WhatIsYourName)
      name <- readLine()
      _ <- println(WelcomeToGame(name))
      _ <- gameLoop(name)
    } yield ()
  }

  def gameLoop[F[_] : Monad : Random : Console](name: String): F[Unit] = {
    for {
      randomNumber <- nextInt(5).map(1 +)
      _ <- println(PleaseGuess(name))
      typedNumber <- readLine()
      _ <- printResults(typedNumber, randomNumber, name)
      continue <- checkContinue(name)
      _ <- if (continue) gameLoop(name)
           else implicitly[Monad[F]].pure(())
    } yield ()
  }

  def printResults[F[_] : Console](input: String, randomNumber: Int, name: String): F[Unit] = {
    parseInt(input).map {
      case `randomNumber` => println(YouGuessedRight(name))
      case _ => println(YouGuessedWrong(name, randomNumber))
    } getOrElse println(ThatIsNotValid(name))
  }

  def checkContinue[F[_] : Monad : Random : Console](name: String): F[Boolean] = {
    for {
      _ <- println(DoYouWantToContinue(name))
      input <- readLine().map(_.toLowerCase)
      continue <- input match {
        case "y" | "yes" => implicitly[Monad[F]].pure(true)
        case "n" | "no" => implicitly[Monad[F]].pure(false)
        case _ => checkContinue(name)
      }
    } yield continue
  }

  def parseInt(s: String): Option[Int] = Try { s.toInt }.toOption

  def println[F[_] : Console](msg: ConsoleOut): F[Unit] = Console[F].println(msg)

  def readLine[F[_] : Console](): F[String] = Console[F].readLine()

  def nextInt[F[_] : Random](upper: Int): F[Int] = Random[F].nextInt(upper)
}
