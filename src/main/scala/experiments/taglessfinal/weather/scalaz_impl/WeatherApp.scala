package experiments.taglessfinal.weather.scalaz_impl

import experiments.taglessfinal.weather.scalaz_impl.City.ErrorHandler
import experiments.taglessfinal.weather.scalaz_impl.Config.ConfigAsk
import experiments.taglessfinal.weather.scalaz_impl.Requests.RequestsState
import scalaz.Monad
import scalaz.syntax.monad._

import scala.language.higherKinds

object WeatherApp {

  def program[F[_] : ConfigAsk : Console : Weather : RequestsState : ErrorHandler : Monad]: F[Unit] = {
    for {
      host <- Config.host
      port <- Config.port
      _ <- implicitly[Console[F]].println(s"Using weather service at http://$host:$port \n")
      _ <- askFetchJudge
    } yield ()
  }

  def askFetchJudge[F[_] : Console : Weather : RequestsState : ErrorHandler : Monad]: F[Unit] = {
    for {
      cityName <- askCity
      city <- City.cityByName(cityName)
      forcast <- fetchForcast(city)
      _ <- implicitly[Console[F]].println(s"Forcast for $city is ${ forcast.temperature }")
      hottest <- Requests.hottestCity
      _ <- implicitly[Console[F]].println(s"Hottest city found so far is $hottest")
    } yield ()
  }

  def askCity[F[_] : Console : Monad]: F[String] = {
    for {
      _ <- implicitly[Console[F]].println("What is the next city")
      cityName <- implicitly[Console[F]].readLine
    } yield cityName
  }

  def fetchForcast[F[_] : Weather : RequestsState : Monad](city: City): F[Forcast] = {
    for {
      maybeForcast <- implicitly[RequestsState[F]].gets(_.get(city))
      forcast <- maybeForcast
        .map(implicitly[Monad[F]].pure(_))
        .getOrElse(implicitly[Weather[F]].forcast(city))
      _ <- implicitly[RequestsState[F]].modify(_ + (city -> forcast))
    } yield forcast
  }
}