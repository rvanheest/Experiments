package monadics.instances.monoids

import monadics.structures.{Equals, Monoid}

trait values {

  implicit val byteIsMonoid: Monoid[Byte] = Monoid.create(0.toByte)((x, y) => (x + y).toByte)
  implicit val shortIsMonoid: Monoid[Short] = Monoid.create(0.toShort)((x, y) => (x + y).toShort)
  implicit val intIsMonoid: Monoid[Int] = Monoid.create(0)(_ + _)
  implicit val longIsMonoid: Monoid[Long] = Monoid.create(0L)(_ + _)
  implicit val stringIsMonoid: Monoid[String] = Monoid.create("")(_ + _)

  implicit val booleanIsEquals: Equals[Boolean] = Equals.natural
  implicit val byteIsEquals: Equals[Byte] = Equals.natural
  implicit val shortIsEquals: Equals[Short] = Equals.natural
  implicit val intIsEquals: Equals[Int] = Equals.natural
  implicit val longIsEquals: Equals[Long] = Equals.natural
  implicit val stringIsEquals: Equals[String] = Equals.natural
}

object values extends values
