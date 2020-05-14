package experiments.taglessfinal.weather

package object scalaz_impl {

  sealed trait TemperatureUnit
  case object Celcius extends TemperatureUnit
  case object Fahrenheit extends TemperatureUnit

  case class Temperature(value: Int, unit: TemperatureUnit = Celcius) {
    override def toString: String = s"$value $unit"
  }

  case class Forcast(temperature: Temperature) {
    override def toString: String = temperature.toString
  }

  sealed trait Error
  case class UnknownCity(city: String) extends Error
}
