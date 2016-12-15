package monadics.test

import monadics.instances.NonEmptyList
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

package object instances {

  implicit def arbNonEmptyList[T](implicit a: Arbitrary[T], as: Arbitrary[List[T]]): Arbitrary[NonEmptyList[T]] = {
    Arbitrary(for {
      t <- arbitrary[T]
      ts <- arbitrary[List[T]]
    } yield NonEmptyList(t, ts))
  }
}
