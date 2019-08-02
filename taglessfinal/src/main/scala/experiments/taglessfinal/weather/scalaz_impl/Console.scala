package experiments.taglessfinal.weather.scalaz_impl

import scala.language.higherKinds

trait Console[F[_]] {
  def println(line: String): F[Unit]

  def readLine: F[String]
}
object Console {
  def apply[F[_]](implicit F: Console[F]): Console[F] = F
}
