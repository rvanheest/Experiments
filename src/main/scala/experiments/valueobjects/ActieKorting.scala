package experiments.valueobjects

import java.time.LocalDate

case class ActieKorting(percentage: Double, einddatum: LocalDate) {
  def toFactor(rekendatum: LocalDate): Double = {
    if (rekendatum.isBefore(einddatum)) (100d - percentage) / 100d
    else 1d
  }
}

object Main extends App {
  println(ActieKorting(10, LocalDate.of(2020, 10, 1)))
  println(ActieKorting(20, LocalDate.of(2020, 10, 1)))
}
