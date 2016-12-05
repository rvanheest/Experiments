import monadics.instances.either._
import monadics.instances.option._

// Either
val l1: Either[String, Int] = Left("abc")
val l2: Either[String, Int] = Left("def")
val r1: Either[String, Int] = Right(1)
val r2: Either[String, Int] = Right(2)
val ol: Either[String, Option[Int]] = Left("ghi")
val or: Either[String, Option[Int]] = Right(Option(3))

// semigroup
l1.orElse(l2) // Left("def")
l1.orElse(r1) // Right(1)
r1.orElse(l1) // Right(1)
r1.orElse(r2) // Right(1)

// functor operators
l1.as(true)            // Left("abc")
r1.as(false)           // Right(false)
l1.void                // Left("abc")
r1.void                // Right(())
l1.zipWith(_ % 2 == 0) // Left("abc")
l2.zipWith(_ % 2 == 0) // Left("def")
r1.zipWith(_ % 2 == 0) // Right((1, false))
r2.zipWith(_ % 2 == 0) // Right((2, true))

// traverse operators
l1.traverse[Option, Int](Option(_))
r1.traverse[Option, Int](Option(_))
ol.sequence
or.sequence
