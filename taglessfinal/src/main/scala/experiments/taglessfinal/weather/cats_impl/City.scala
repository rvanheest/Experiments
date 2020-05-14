package experiments.taglessfinal.weather.cats_impl

import cats.Applicative
import cats.mtl.FunctorRaise
import cats.mtl.syntax.raise._
import cats.syntax.applicative._

import scala.language.higherKinds

case class City(name: String) {
  override def toString: String = name
}
object City {
  type ErrorHandler[F[_]] = FunctorRaise[F, Error]

  def ErrorHandler[F[_]](implicit F: ErrorHandler[F]): ErrorHandler[F] = F

  def cityByName[F[_] : Applicative : ErrorHandler](cityName: String): F[City] = {
    cityName match {
      case "Rotterdam" => City(cityName).pure
      case "Den Haag" => City(cityName).pure
      case _ => unknownCity(cityName).raise
    }
  }
}
