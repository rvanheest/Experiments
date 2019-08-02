package experiments.taglessfinal.weather.cats_impl

import cats.mtl.MonadState

import scala.language.higherKinds

object Requests {

  type Requests = Map[City, Forcast]

  def empty: Requests = Map.empty

  def hottest(requests: Requests): (City, Forcast) = {
    requests.maxBy { case (_, Forcast(Temperature(value, _))) => value }
  }

  type RequestsState[F[_]] = MonadState[F, Requests]

  def RequestsState[F[_]](implicit F: RequestsState[F]): MonadState[F, Requests] = F

  def hottestCity[F[_] : RequestsState]: F[(City, Temperature)] = {
    RequestsState[F].inspect(requests => {
      val (city, Forcast(temperature)) = Requests.hottest(requests)
      (city, temperature)
    })
  }
}
