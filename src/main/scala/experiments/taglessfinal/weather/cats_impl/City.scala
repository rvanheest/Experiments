package experiments.taglessfinal.weather.cats_impl

import cats.Applicative
import cats.mtl.FunctorRaise

import scala.language.higherKinds

case class City(name: String) {
  override def toString: String = name
}
object City {
  type ErrorHandler[F[_]] = FunctorRaise[F, Error]

  def ErrorHandler[F[_]](implicit F: ErrorHandler[F]): ErrorHandler[F] = F

  def cityByName[F[_] : Applicative : ErrorHandler](cityName: String): F[City] = {
    cityName match {
      case "Rotterdam" => Applicative[F].pure(City(cityName))
      case "Den Haag" => Applicative[F].pure(City(cityName))
      case _ => ErrorHandler[F].raise(UnknownCity(cityName))
    }
  }
}
