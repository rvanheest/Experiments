package experiments.taglessfinal.effect.cats

import scala.language.higherKinds

trait Random[F[_]] {
  def nextInt(upper: Int): F[Int]
}
object Random {
  def apply[F[_]: Random]: Random[F] = implicitly[Random[F]]
}
