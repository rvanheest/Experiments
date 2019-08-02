package experiments.taglessfinal.weather.cats_impl

import scala.language.higherKinds

// third party client
class WeatherClient(host: String, port: Int) {

  def forcast(city: City): Forcast = city match {
    case City("Rotterdam") => Forcast(Temperature(20))
    case City("Den Haag") => Forcast(Temperature(25))
  }
}

trait Weather[F[_]] {
  def forcast(city: City): F[Forcast]
}
object Weather {
  def apply[F[_]](implicit F: Weather[F]): Weather[F] = F
}

