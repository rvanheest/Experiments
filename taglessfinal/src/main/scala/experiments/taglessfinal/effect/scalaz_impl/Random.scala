package experiments.taglessfinal.effect.scalaz_impl

import scala.language.higherKinds

trait Random[F[_]] {
  def nextInt(upper: Int): F[Int]
}
object Random {
  def apply[F[_]: Random]: Random[F] = implicitly[Random[F]]
}
