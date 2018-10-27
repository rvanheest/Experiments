package experiments.taglessfinal.effect

import scala.io.StdIn
import scala.util.{ Random, Try }

object FixBugs extends App {

  runApplication()

  def runApplication(): Unit = {
    println("What is your name?")

    val name = StdIn.readLine()

    println(s"Hello, $name, welcome to the game!")

    var exec = true

    while (exec) {
      val randomNumber = Random.nextInt(5) + 1

      println(s"Dear $name, please guess a number from 1 to 5:")

      val guess = parseInt(StdIn.readLine())

      guess match {
        case Some(`randomNumber`) => println(s"You guessed right, $name!")
        case Some(_) => println(s"You guessed wrong, $name! The number was: $randomNumber")
        case None => println("You did not enter a number")
      }

      var continue = true

      while (continue) {
        continue = false

        println(s"Do you want to continue, $name? [y/n]")

        StdIn.readLine().toLowerCase match {
          case "y" | "yes" => exec = true
          case "n" | "no" => exec = false
          case _ => continue = true
        }
      }
    }
  }

  def parseInt(s: String): Option[Int] = Try { s.toInt }.toOption
}
