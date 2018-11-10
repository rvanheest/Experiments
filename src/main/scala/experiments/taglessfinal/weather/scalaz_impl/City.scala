package experiments.taglessfinal.weather.scalaz_impl

import scalaz.Applicative

import scala.language.higherKinds

case class City(name: String) {
  override def toString: String = name
}
object City {
  type ErrorHandler[F[_]] = ApplicativeError[F, Error]

  def cityByName[F[_] : Applicative : ErrorHandler](cityName: String): F[City] = {
    cityName match {
      case "Rotterdam" => implicitly[Applicative[F]].pure(City(cityName))
      case "Den Haag" => implicitly[Applicative[F]].pure(City(cityName))
      case _ => implicitly[ErrorHandler[F]].raiseError(UnknownCity(cityName))
    }
  }
}

trait ApplicativeError[F[_], E] {
  val applicative: Applicative[F]

  def raiseError[A](e: E): F[A]
}
