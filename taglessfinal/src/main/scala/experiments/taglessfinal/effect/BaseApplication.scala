package experiments.taglessfinal.effect

import scala.io.StdIn
import scala.util.Random

object BaseApplication extends App {

  runApplication()

  def runApplication(): Unit = {
    println("What is your name?")

    val name = StdIn.readLine()

    println(s"Hello, $name, welcome to the game!")

    var exec = true

    while (exec) {
      val randomNumber = Random.nextInt(5) + 1

      println(s"Dear $name, please guess a number from 1 to 5:")

      val guess = StdIn.readLine().toInt

      if (guess == randomNumber) println(s"You guessed right, $name!")
      else println(s"You guessed wrong, $name! The number was: $randomNumber")

      println(s"Do you want to continue, $name?")

      StdIn.readLine() match {
        case "y" => exec = true
        case "n" => exec = false
      }
    }
  }
}
