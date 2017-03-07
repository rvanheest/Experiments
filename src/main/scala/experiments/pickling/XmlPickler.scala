package experiments.pickling

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.xml.{Attribute, Node}

trait XmlPickler {

//	case class State(elems: Seq[Node], nesting: Int, pickleElem: Boolean)
//
//	type Pickler[A] = (A, State) => State
//
//	type UnpickleErr = (String, State)
//	type UnpickleVal[A] = Either[UnpickleErr, A]
//	abstract class Unpickler[A] {
//		def unpickle(state: State): (UnpickleVal[A], State)
//
//		def map[B](f: A => B): Unpickler[B] = Unpickler(state => {
//			val (res, elem2) = unpickle(state)
//			(res.right.map(f), elem2)
//		})
//
//		def flatMap[B](f: A => Unpickler[B]): Unpickler[B] = Unpickler(state => {
//			val (res, state2) = unpickle(state)
//			res.fold(err => (Left(err), state2), v => f(v).unpickle(state2))
//		})
//
//		def orElse[B](f: A => Unpickler[B], v: Unpickler[B]): Unpickler[B] = Unpickler(state => {
//			val (res, state2) = unpickle(state)
//
//			res.fold({
//				case err@(msg, state3) => if (state3.nesting == state.nesting) v.unpickle(state)
//				else (Left(err), state2)
//			}, a => f(a).unpickle(state2))
//		})
//	}
//	object Unpickler {
//		def apply[A](runUP: State => (UnpickleVal[A], State)): Unpickler[A] = new Unpickler[A] {
//			override def unpickle(state: State): (UnpickleVal[A], State) = runUP(state)
//		}
//
//		def from[A](a: A): Unpickler[A] = Unpickler((Right(a), _))
//
//		def fail[A](msg: String): Unpickler[A] = Unpickler(state => (Left(msg, state), state))
//
//		def get: Unpickler[State] = Unpickler(state => (Right(state), state))
//
//		def put(state: State): Unpickler[Unit] = Unpickler(_ => (Right(()), state))
//
//		def state[A](f: State => (A, State)): Unpickler[A] = {
//			for {
//				state <- get
//				(a, state2) = f(state)
//				_ <- put(state2)
//			} yield a
//		}
//
//		def gets[A](f: State => A): Unpickler[A] = get.map(f)
//
//		def modify(f: State => State): Unpickler[Unit] = state(s => ((), f(s)))
//
//		def liftOption[A](err: String, opt: Option[A]): Unpickler[A] = opt.map(from).getOrElse(fail(err))
//
//		def liftUnpickleVal[A](v: UnpickleVal[A]): Unpickler[A] = Unpickler((v, _))
//	}
//
//	object XmlUnpickler {
//		def getContent: Unpickler[Node] = {
//			Unpickler.gets(_.elems)
//			  .flatMap(seq => seq.headOption
//					.map(node => Unpickler.modify(_.copy(elems = seq.tail)).map(_ => node))
//					.getOrElse(Unpickler.fail("no more content to be read")))
//		}
//
//		private def findElem[A](seq: Seq[A])(f: A => Boolean): Option[(A, Seq[A])] = {
//
//			@tailrec
//			def find[X](xs: Seq[X])(prefix: Seq[X] => Seq[X]): Option[(X, Seq[X])] = {
//				xs match {
//					case Seq.empty => Option.empty
//					case Seq(x, xs@_*) if f(x) => Option(x, prefix(xs))
//					case Seq(x, xs@_*) => find(xs)(prefix.compose(x +: _))
//				}
//			}
//
//			find(seq)(identity)
//		}
//
//		def getAttribute(name: String): Unpickler[Node] = {
//			def findAtt(seq: Seq[Node]): Option[(Node, Seq[Node])] = {
//				findElem(seq)(node => {
//
//					???
//				})
//			}
//
//			Unpickler.gets(_.elems)
//			  .flatMap(seq => {
//					findAtt(seq)
//					  .map { case (n, ns) => Unpickler.modify(_.copy(elems = ns)).map(_ => n) }
//					  .getOrElse(Unpickler.fail(s"no attribute value found for $name"))
//				})
//		}
//	}

//	case class PU[A](pickle: Pickler[A],
//									 unpickle: Unpickler[A]) {
//		def seq[B](f: B => A, g: A => PU[B]): PU[B] = PU(
//			pickle = (b: B, elem: Elem) => {
//				val a: A = f(b)
//				g(a).pickle(b, this.pickle(a, elem))
//			},
//			unpickle = this.unpickle.flatMap(g(_).unpickle)
//		)
//
//		def wrapOption[B](f: A => Option[B], g: B => A): PU[B] = {
//			seq(g, f andThen PU.liftOption)
//		}
//
//		def orElse[B >: A](pb: PU[B])(f: A => PU[B]): Unpickler[B] = {
//			def mchoice[X, Y](px: Unpickler[X], py: Unpickler[Y])(f: X => Unpickler[Y]): Unpickler[Y] = {
//				Unpickler(elem => {
//					val (r, elem2) = px.unpickle(elem)
//					r.map(x => f(x).unpickle(elem2))
//
//					???
//				})
//			}
//		}
//	}
//	object PU {
//		def fail[A]: PU[A] = PU(
//			pickle = (a, elem) => elem,
//			unpickle = Unpickler((Option.empty, _))
//		)
//
//		def empty: PU[Unit] = lift(())
//
//		def lift[A](a: A): PU[A] = PU(
//			pickle = (_, elem) => elem,
//			unpickle = Unpickler(elem => (Option(a), elem))
//		)
//
//		def liftOption[A](opt: Option[A]): PU[A] = opt.map(lift).getOrElse(fail)
//	}

//	trait XmlPickler[A] {
//		def xmlPickle: PU[A]
//	}

//	object IntPickler extends XmlPickler[Int] {
//		override def xmlPickle: PU[Int] = {
//
//
//			/*
//		  xpPrim                  :: (Read a, Show a) => PU a
//			xpPrim                  = xpWrapEither (readMaybe, show) xpText
//					where
//					readMaybe           :: Read a => String -> Either String a
//					readMaybe str       = val (reads str)
//							where
//								val [(x,"")]  = Right x
//								val _         = Left $ "xpPrim: reading string " ++ show str ++ " failed"
//
//			xpWrapEither             :: (a -> Either String b, b -> a) -> PU a -> PU b
//			xpWrapEither (i, j) pa   = (xpSeq j pa (xpLiftEither . i)) { theSchema = theSchema pa }
//		 */
//
//
//
//			???
//		}
//	}

}
