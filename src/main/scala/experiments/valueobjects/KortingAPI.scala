package experiments.valueobjects

case class Factor(waarde: Double) {
  assert(waarde >= 0d, s"Ongeldige factor $waarde")

  def *(other: Factor): Factor = Factor(waarde * other.waarde)
  def /(other: Factor): Factor = Factor(waarde / other.waarde)
  def *(bedrag: Double): Double = waarde * bedrag
  def /(bedrag: Double): Double = waarde / bedrag
}

object Factor {
  implicit class FactorOps(val bedrag: Double) extends AnyVal {
    def *(factor: Factor): Double = bedrag * factor.waarde
    def /(factor: Factor): Double = bedrag / factor.waarde
  }
}

sealed abstract class Korting(percentage: Double) {
  assert(percentage >= 0d && percentage <= 100d, s"Ongeldige korting $percentage")
  
  def toFactor: Factor = Factor((100d - this.percentage) / 100d)
}

case class AcceptatieKorting(percentage: Double) extends Korting(percentage)
case class TermijnToeslag(percentage: Double) extends Korting(percentage) {
  override def toFactor: Factor = Factor((100d + this.percentage) / 100d)
}

object Demo extends App {
  val accKorting = AcceptatieKorting(10d)
  println(accKorting.toFactor * accKorting.toFactor)
  println(15d * accKorting.toFactor)
  println(accKorting.toFactor * 15d)
}
