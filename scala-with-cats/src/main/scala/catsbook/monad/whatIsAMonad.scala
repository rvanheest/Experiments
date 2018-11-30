package catsbook.monad

import scala.util.Try

object whatIsAMonad extends App {

  def parseInt(str: String): Option[Int] = Try { str.toInt }.toOption

  def divide(a: Int, b: Int): Option[Int] = if (b == 0) None
                                            else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] = {
    parseInt(aStr).flatMap(aNum =>
      parseInt(bStr).flatMap(bNum =>
        divide(aNum, bNum)
      )
    )
  }

  def stringDivideBy2(aStr: String, bStr: String): Option[Int] = {
    for {
      aNum <- parseInt(aStr)
      bNum <- parseInt(bStr)
      result <- divide(aNum, bNum)
    } yield result
  }

  println(stringDivideBy("6", "2")) // Some(3)
  println(stringDivideBy("6", "0")) // None
  println(stringDivideBy("6", "foo")) // None
  println(stringDivideBy("bar", "2")) // None

  // calculate combinations when using List Monad
  println {
    for {
      x <- (1 to 3).toList
      y <- (4 to 5).toList
    } yield (x, y)
  }


}
