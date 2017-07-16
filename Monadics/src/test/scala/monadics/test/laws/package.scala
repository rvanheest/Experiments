package monadics.test

import monadics.instances._
import monadics.instances.either._
import monadics.instances.monoids._
import monadics.structures.{Equals, MonadPlus, Monoid}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}

import scala.language.higherKinds

package object laws {

	implicit def function1IsEquals[A, B](implicit arbA: Arbitrary[A], eqB: Equals[B]): Equals[A => B] = {
		Equals.create((x, y) => {
			val samples = Stream.continually(arbA.arbitrary.sample).flatten.take(100)
			samples.forall(a => eqB.equals(x(a), y(a)))
		})
	}

	implicit def stateTFunctionIsEquals[S, A, M[+_]](implicit arbS: Arbitrary[S], eqMAS: Equals[M[(A, S)]]): Equals[S => M[(A, S)]] = {
		Equals.create((x, y) => {
			val samples = Stream.continually(arbS.arbitrary.sample).flatten.take(100)
			samples.forall(s => eqMAS.equals(x(s), y(s)))
		})
	}

	implicit def tupleIsEquals[A, B](implicit aEquals: Equals[A], bEquals: Equals[B]): Equals[(A, B)] = {
		Equals.create { case ((a1, b1), (a2, b2)) => aEquals.equals(a1, a2) && bEquals.equals(b1, b2) }
	}

	implicit def arbIdentity[T](implicit a: Arbitrary[T]): Arbitrary[Identity[T]] = {
		Arbitrary(arbitrary[T].map(Identity(_)))
	}

	implicit def arbLeftEither[L, R](implicit a: Arbitrary[Either[L, R]]): Arbitrary[LeftEither[L, R]] = {
		Arbitrary(arbitrary[Either[L, R]].map(_.left))
	}

	implicit def arbRightEither[L, R](implicit a: Arbitrary[Either[L, R]]): Arbitrary[RightEither[L, R]] = {
		Arbitrary(arbitrary[Either[L, R]].map(_.right))
	}

	implicit def arbOptionTOfList[T](implicit a: Arbitrary[T], monad: MonadPlus[OptionT[List, ?]]): Arbitrary[OptionT[List, T]] = {
		Arbitrary(arbitrary[T].map(OptionT.create[List, T]))
	}

	implicit def arbState[T](implicit a: Arbitrary[T]): Arbitrary[State[Int, T]] = {
		Arbitrary(arbitrary[T].map(t => new State[Int, T]((t, _))))
	}

	implicit def arbStateTOfList[T](implicit a: Arbitrary[T], monad: MonadPlus[StateT[Int, ?, List]]): Arbitrary[StateT[Int, T, List]] = {
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

	implicit def arbAny: Arbitrary[Any] = {
		Arbitrary(arbitrary[Boolean].map(Any(_)))
	}

	implicit def arbAll: Arbitrary[All] = {
		Arbitrary(arbitrary[Boolean].map(All(_)))
	}

	implicit def arbDual[T](implicit tMonoid: Monoid[T], a: Arbitrary[T]): Arbitrary[Dual[T]] = {
		Arbitrary(arbitrary[T].map(Dual(_)))
	}

	implicit def arbEndo[T](implicit a: Arbitrary[T => T]): Arbitrary[Endo[T]] = {
		Arbitrary(arbitrary[T => T].map(Endo(_)))
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

	implicit def arbReader[T](implicit a: Arbitrary[T]): Arbitrary[Reader[Int, T]] = {
		Arbitrary(arbitrary[T].map(t => new Reader(_ => t)))
	}

	implicit def arbContinuation[T](implicit a: Arbitrary[T]): Arbitrary[Continuation[Int, T]] = {
		Arbitrary(arbitrary[T].map(t => new Continuation(f => f(t))))
	}
}
