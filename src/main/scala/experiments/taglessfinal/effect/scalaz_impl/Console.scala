package experiments.taglessfinal.effect.scalaz_impl

import scala.language.higherKinds

trait Console[F[_]] {
  def println(s: ConsoleOut): F[Unit]

  def readLine(): F[String]
}
object Console {
  def apply[F[_]: Console]: Console[F] = implicitly[Console[F]]
}
