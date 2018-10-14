package experiments.effect.structuredMessages

sealed trait ConsoleOut {
  def en: String
}

object ConsoleOut {
  case object WhatIsYourName extends ConsoleOut {
    def en = "What is your name?"
  }
  case class WelcomeToGame(name: String) extends ConsoleOut {
    def en: String = s"Hello, $name, welcome to the game!"
  }
  case class PleaseGuess(name: String) extends ConsoleOut {
    def en: String = s"Dear $name, please guess a number from 1 to 5:"
  }
  case class ThatIsNotValid(name: String) extends ConsoleOut {
    def en: String = s"You did not enter a number, $name!"
  }
  case class YouGuessedRight(name: String) extends ConsoleOut {
    def en: String = s"You guessed right, $name!"
  }
  case class YouGuessedWrong(name: String, num: Int) extends ConsoleOut {
    def en: String = s"You guessed wrong, $name! The number was: $num."
  }
  case class DoYouWantToContinue(name: String) extends ConsoleOut {
    def en: String = s"Do you want to continue, $name? [y/n]"
  }
}
