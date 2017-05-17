package monadics.test

import monadics.instances.{Identity, NonEmptyList, Reader}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

package object instances {

  implicit def arbNonEmptyList[T](implicit a: Arbitrary[T], as: Arbitrary[List[T]]): Arbitrary[NonEmptyList[T]] = {
    Arbitrary(for {
      t <- arbitrary[T]
      ts <- arbitrary[List[T]]
    } yield NonEmptyList(t, ts))
  }

	implicit def arbIdentity[T](implicit a: Arbitrary[T]): Arbitrary[Identity[T]] = {
		Arbitrary(arbitrary[T].map(Identity(_)))
	}

	implicit def arbReader[T, R](implicit a: Arbitrary[T]): Arbitrary[Reader[R, T]] = {
		Arbitrary(arbitrary[T].map(t => new Reader(_ => t)))
	}
}
