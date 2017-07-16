import monadics.instances.either._
import monadics.instances.option._

import scala.util.Either.RightProjection

// RightEither
val rl1: RightProjection[String, Int] = Left("abc").right
val rl2: RightProjection[String, Int] = Left("def").right
val rr1: RightProjection[String, Int] = Right(1).right
val rr2: RightProjection[String, Int] = Right(2).right
val ol: RightProjection[String, Option[Int]] = Left("ghi").right
val or: RightProjection[String, Option[Int]] = Right(Option(3)).right

// semigroup
rl1.orElse(rl2).e // Left("def")
rl1.orElse(rr1).e // Right(1)
rr1.orElse(rl1).e // Right(1)
rr1.orElse(rr2).e // Right(1)

// functor operators
rl1.as(true).e            // Left("abc")
rr1.as(false).e           // Right(false)
rl1.void.e                // Left("abc")
rr1.void.e                // Right(())
rl1.zipWith(_ % 2 == 0).e // Left("abc")
rl2.zipWith(_ % 2 == 0).e // Left("def")
rr1.zipWith(_ % 2 == 0).e // Right((1, false))
rr2.zipWith(_ % 2 == 0).e // Right((2, true))

// traverse operators
rl1.traverse[Option, Int](Option(_)).map(_.e)
rr1.traverse[Option, Int](Option(_)).map(_.e)
ol.sequence.map(_.e)
or.sequence.map(_.e)
