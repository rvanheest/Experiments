import monadics.instances.either._
import monadics.instances.option._

import scala.util.Either.LeftProjection

// LeftEither
val ll1: LeftProjection[String, Int] = Left("abc").left
val ll2: LeftProjection[String, Int] = Left("def").left
val lr1: LeftProjection[String, Int] = Right(1).left
val lr2: LeftProjection[String, Int] = Right(2).left
val ol: LeftProjection[Option[String], Int] = Left(Option("ghi")).left
val or: LeftProjection[Option[String], Int] = Right(3).left

// semigroup
ll1.orElse(ll2).e // Left("abc")
ll1.orElse(lr1).e // Left("abc")
lr1.orElse(ll1).e // Left("abc")
lr1.orElse(lr2).e // Right(2)

// functor operators
ll1.as(true).e                   // Left(true)
lr1.as(false).e                  // Right(1)
ll1.void.e                       // Left(())
lr1.void.e                       // Right(1)
ll1.zipWith(_.startsWith("a")).e // Left(("abc", true))
ll2.zipWith(_.startsWith("a")).e // Left(("def", false))
lr1.zipWith(_.startsWith("a")).e // Right(1)
lr2.zipWith(_.startsWith("a")).e // Right(2)

// traverse operators
ll1.traverse[Option, String](Option(_)).map(_.e)
lr1.traverse[Option, String](Option(_)).map(_.e)
ol.sequence.map(_.e)
or.sequence.map(_.e)
