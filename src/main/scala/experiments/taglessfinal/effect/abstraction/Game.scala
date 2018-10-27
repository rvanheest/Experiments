package experiments.taglessfinal.effect.abstraction

import experiments.taglessfinal.effect.abstraction.Program.{ ProgramSyntax, from }

import scala.language.{ higherKinds, postfixOps }
import scala.util.Try

object Game {

  def runApplication[F[_] : Program : Random : Console]: F[Unit] = {
    for {
      _ <- println("What is your name?")
      name <- readLine()
      _ <- println(s"Hello, $name, welcome to the game!")
      _ <- gameLoop(name)
    } yield ()
  }

  def gameLoop[F[_] : Program : Random : Console](name: String): F[Unit] = {
    for {
      randomNumber <- nextInt(5).map(1 +)
      _ <- println(s"Dear $name, please guess a number from 1 to 5:")
      typedNumber <- readLine()
      _ <- printResults(typedNumber, randomNumber, name)
      continue <- checkContinue(name)
      _ <- if (continue) gameLoop(name)
           else from(())
    } yield ()
  }

  def printResults[F[_] : Console](input: String, randomNumber: Int, name: String): F[Unit] = {
    parseInt(input).map {
      case `randomNumber` => println(s"You guessed right, $name!")
      case _ => println(s"You guessed wrong, $name! The number was: $randomNumber")
    } getOrElse println("You did not enter a number")
  }

  def checkContinue[F[_] : Program : Random : Console](name: String): F[Boolean] = {
    for {
      _ <- println(s"Do you want to continue, $name? [y/n]")
      input <- readLine().map(_.toLowerCase)
      continue <- input match {
        case "y" | "yes" => from(true)
        case "n" | "no" => from(false)
        case _ => checkContinue(name)
      }
    } yield continue
  }

  def parseInt(s: String): Option[Int] = Try { s.toInt }.toOption

  def println[F[_] : Console](s: String): F[Unit] = Console[F].println(s)

  def readLine[F[_] : Console](): F[String] = Console[F].readLine()

  def nextInt[F[_] : Random](upper: Int): F[Int] = Random[F].nextInt(upper)
}
