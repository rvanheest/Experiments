package experiments.taglessfinal.weather.scalaz_impl

import scalaz.Applicative
import scalaz.syntax.applicative._

import scala.language.higherKinds

case class City(name: String) {
  override def toString: String = name
}
object City {
  type ErrorHandler[F[_]] = ApplicativeError[F, Error]
  def ErrorHandler[F[_]](implicit F: ErrorHandler[F]): ErrorHandler[F] = F

  def cityByName[F[_] : Applicative : ErrorHandler](cityName: String): F[City] = {
    cityName match {
      case "Rotterdam" => City(cityName).pure
      case "Den Haag" => City(cityName).pure
      case _ => ErrorHandler[F].raiseError(UnknownCity(cityName))
    }
  }
}

trait ApplicativeError[F[_], E] {
  val applicative: Applicative[F]

  def raiseError[A](e: E): F[A]
}
object ApplicativeError {
  def apply[F[_], E](implicit F: ApplicativeError[F, E]): ApplicativeError[F, E] = F
}
