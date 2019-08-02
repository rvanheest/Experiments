package experiments.taglessfinal.effect.monadic

import scala.io.StdIn
import scala.language.postfixOps
import scala.util.{ Random, Try }

object Game {

  def runApplication: IO[Unit] = {
    for {
      _ <- println("What is your name?")
      name <- readLine()
      _ <- println(s"Hello, $name, welcome to the game!")
      _ <- gameLoop(name)
    } yield ()
  }

  def gameLoop(name: String): IO[Unit] = {
    for {
      randomNumber <- nextInt(5).map(1 +)
      _ <- println(s"Dear $name, please guess a number from 1 to 5:")
      typedNumber <- readLine()
      _ <- printResults(typedNumber, randomNumber, name)
      continue <- checkContinue(name)
      _ <- if (continue) gameLoop(name)
           else IO.from(())
    } yield ()
  }

  def printResults(input: String, randomNumber: Int, name: String): IO[Unit] = {
    parseInt(input).map {
      case `randomNumber` => println(s"You guessed right, $name!")
      case _ => println(s"You guessed wrong, $name! The number was: $randomNumber")
    } getOrElse println("You did not enter a number")
  }

  def checkContinue(name: String): IO[Boolean] = {
    for {
      _ <- println(s"Do you want to continue, $name? [y/n]")
      input <- readLine().map(_.toLowerCase)
      continue <- input match {
        case "y" | "yes" => IO.from(true)
        case "n" | "no" => IO.from(false)
        case _ => checkContinue(name)
      }
    } yield continue
  }

  def parseInt(s: String): Option[Int] = Try { s.toInt }.toOption

  def println(s: String): IO[Unit] = IO(() => Console.println(s))

  def readLine(): IO[String] = IO(() => StdIn.readLine())

  def nextInt(upper: Int): IO[Int] = IO(() => Random.nextInt(upper))
}
