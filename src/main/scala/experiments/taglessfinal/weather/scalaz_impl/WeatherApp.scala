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
      _ <- Console[F].println(s"Using weather service at http://$host:$port \n")
      _ <- askFetchJudge
    } yield ()
  }

  def askFetchJudge[F[_] : Console : Weather : RequestsState : ErrorHandler : Monad]: F[Unit] = {
    for {
      cityName <- askCity
      city <- City.cityByName(cityName)
      forcast <- fetchForcast(city)
      _ <- Console[F].println(s"Forcast for $city is $forcast")
      hottest <- Requests.hottestCity
      (hottestCity, highestTemperature) = hottest
      _ <- Console[F].println(s"Hottest city found so far is $hottestCity with $highestTemperature")
    } yield ()
  }

  def askCity[F[_] : Console : Monad]: F[String] = {
    for {
      _ <- Console[F].println("What is the next city")
      cityName <- Console[F].readLine
    } yield cityName
  }

  def fetchForcast[F[_] : Weather : RequestsState : Monad](city: City): F[Forcast] = {
    for {
      maybeForcast <- RequestsState[F].gets(_.get(city))
      forcast <- maybeForcast.fold(Weather[F].forcast(city))(_.pure)
      _ <- RequestsState[F].modify(_ + (city -> forcast))
    } yield forcast
  }
}
