package monadics.instances.monoids

import monadics.structures.Monoid

trait values {

  implicit val byteIsMonoid: Monoid[Byte] = Monoid.create(0.toByte)((x, y) => (x + y).toByte)
  implicit val shortIsMonoid: Monoid[Short] = Monoid.create(0.toShort)((x, y) => (x + y).toShort)
  implicit val intIsMonoid: Monoid[Int] = Monoid.create(0)(_ + _)
  implicit val longIsMonoid: Monoid[Long] = Monoid.create(0L)(_ + _)
  implicit val stringIsMonoid: Monoid[String] = Monoid.create("")(_ + _)
}

object values extends values
