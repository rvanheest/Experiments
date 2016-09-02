package monadics.test

import monadics.instances.OptionT.OptionTMonadPlus
import monadics.instances.StateT.StateTMonadPlus
import monadics.instances._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

package object laws {

	implicit def arbIdentity[T](implicit a: Arbitrary[T]): Arbitrary[Identity[T]] = {
		Arbitrary(arbitrary[T].map(Identity(_)))
	}

	implicit def arbOptionTOfList[T](implicit a: Arbitrary[T], monad: OptionTMonadPlus[List]): Arbitrary[OptionT[List, T]] = {
		Arbitrary(arbitrary[T].map(OptionT.create[List, T]))
	}

	implicit def arbState[T](implicit a: Arbitrary[T]): Arbitrary[State[Int, T]] = {
		Arbitrary(arbitrary[T].map(t => new State[Int, T]((t, _))))
	}

	implicit def arbStateTOfList[T](implicit a: Arbitrary[T], monad: StateTMonadPlus[Int, List]): Arbitrary[StateT[Int, T, List]] = {
		Arbitrary(arbitrary[T].map(StateT.from[Int, T, List]))
	}

	implicit def arbTree[T](implicit a: Arbitrary[T]): Arbitrary[Tree[T]] = {
		Arbitrary(Gen.lzy(Gen.oneOf(arbitrary[T].map(Leaf(_)), arbitrary[(Tree[T], Tree[T])].map((Branch[T] _).tupled))))
	}
}
