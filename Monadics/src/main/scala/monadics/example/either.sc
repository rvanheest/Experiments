import monadics.instances.either._

import scala.util.Either.{LeftProjection, RightProjection}

// Either
val l1: Either[String, Int] = Left("abc")
val l2: Either[String, Int] = Left("def")
val r1: Either[String, Int] = Right(3)
val r2: Either[String, Int] = Right(4)

// semigroup
l1.orElse(l2) // Left("def")
l1.orElse(r1) // Right(3)
r1.orElse(l1) // Right(3)
r1.orElse(r2) // Right(3)

// functor operators
l1.as(true)            // Left("abc")
r1.as(false)           // Right(false)
l1.void                // Left("abc")
r1.void                // Right(())
l1.zipWith(_ % 2 == 0) // Left("abc")
l2.zipWith(_ % 2 == 0) // Left("def")
r1.zipWith(_ % 2 == 0) // Right((3, false))
r2.zipWith(_ % 2 == 0) // Right((4, true))

// LeftEither
val ll1: LeftProjection[String, Int] = Left("abc").left
val ll2: LeftProjection[String, Int] = Left("def").left
val lr1: LeftProjection[String, Int] = Right(3).left
val lr2: LeftProjection[String, Int] = Right(4).left

// semigroup
ll1.orElse(ll2).e // Left("abc")
ll1.orElse(lr1).e // Left("abc")
lr1.orElse(ll1).e // Left("abc")
lr1.orElse(lr2).e // Right(4)

// functor operators
ll1.as(true).e                   // Left(true)
lr1.as(false).e                  // Right(3)
ll1.void.e                       // Left(())
lr1.void.e                       // Right(3)
ll1.zipWith(_.startsWith("a")).e // Left(("abc", true))
ll2.zipWith(_.startsWith("a")).e // Left(("def", false))
lr1.zipWith(_.startsWith("a")).e // Right(3)
lr2.zipWith(_.startsWith("a")).e // Right(4)

// RightEither
val rl1: RightProjection[String, Int] = Left("abc").right
val rl2: RightProjection[String, Int] = Left("def").right
val rr1: RightProjection[String, Int] = Right(3).right
val rr2: RightProjection[String, Int] = Right(4).right

// semigroup
rl1.orElse(rl2).e // Left("def")
rl1.orElse(rr1).e // Right(3)
rr1.orElse(rl1).e // Right(3)
rr1.orElse(rr2).e // Right(3)

// functor operators
rl1.as(true).e            // Left("abc")
rr1.as(false).e           // Right(false)
rl1.void.e                // Left("abc")
rr1.void.e                // Right(())
rl1.zipWith(_ % 2 == 0).e // Left("abc")
rl2.zipWith(_ % 2 == 0).e // Left("def")
rr1.zipWith(_ % 2 == 0).e // Right((3, false))
rr2.zipWith(_ % 2 == 0).e // Right((4, true))
