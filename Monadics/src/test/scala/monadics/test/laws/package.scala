package monadics.test

import monadics.instances.OptionT.OptionTMonadPlus
import monadics.instances.StateT.StateTMonadPlus
import monadics.instances._
import monadics.structures.Monoid
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}

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

	implicit def arbSum[T](implicit numeric: Numeric[T], a: Arbitrary[T]): Arbitrary[Sum[T]] = {
		Arbitrary(arbitrary[T].map(Sum(_)))
	}

	implicit def arbProduct[T](implicit numeric: Numeric[T], a: Arbitrary[T]): Arbitrary[Product[T]] = {
		Arbitrary(arbitrary[T].map(Product(_)))
	}

	implicit def arbNonEmptyList[T](implicit a: Arbitrary[T], as: Arbitrary[List[T]]): Arbitrary[NonEmptyList[T]] = {
		Arbitrary(for {
			t <- arbitrary[T]
			ts <- arbitrary[List[T]]
		} yield NonEmptyList(t, ts))
	}

	implicit def arbWriter[T](implicit a: Arbitrary[T], monoid: Monoid[String]): Arbitrary[Writer[String, T]] = {
		Arbitrary(for {
			t <- arbitrary[T]
			log <- arbitrary[String]
		} yield Writer[String, T](t, log))
	}
}
