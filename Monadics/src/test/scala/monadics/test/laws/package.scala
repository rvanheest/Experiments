package monadics.test

import monadics.instances.OptionT.OptionTMonadPlus
import monadics.instances.StateT.StateTMonadPlus
import monadics.instances._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._

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
		val ts = arbitrary[T]

		def leafs: Gen[Leaf[T]] = ts.map(Leaf(_))

		def genBranch(maxDepth: Int): Gen[Branch[T]] = for {
			depthL <- Gen.choose(0, maxDepth - 1)
			depthR <- Gen.choose(0, maxDepth - 1)
			left <- genTree(depthL)
			right <- genTree(depthR)
		} yield Branch(left, right)

		def genTree(maxDepth: Int): Gen[Tree[T]] = {
			if (maxDepth == 0) leafs
			else Gen.oneOf(leafs, genBranch(maxDepth))
		}

		Arbitrary(Gen.sized { size => genTree(size) })
	}
}
