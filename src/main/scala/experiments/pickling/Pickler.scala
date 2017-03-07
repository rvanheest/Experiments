package experiments.pickling

trait Pickler {

	type State

	def emptyState: State

	class PU[A](private[PU] val pickler: (A, State) => State,
							private[PU] val unpickler: State => (A, State)) {
		self =>

		def pickle(a: A): State = pickler(a, emptyState)

		def unpickle(state: State): A = unpickler(state)._1

		def seq[B](f: B => A)(g: A => PU[B]): PU[B] = new PU(
			(b: B, s: State) => {
				val a = f(b)
				self.pickler(a, g(a).pickler(b, s))
			},
			(state: State) => {
				val (a, state2) = self.unpickler(state)
				g(a).unpickler(state2)
			}
		)

		def pair[B](pb: PU[B]): PU[(A, B)] = {
			this.seq[(A, B)]({ case (a, _) => a })(a =>
				pb.seq[(A, B)]({ case (_, b) => b })(b =>
					PU.lift(a, b)))
		}

		def triple[B, C](pb: PU[B], pc: PU[C]): PU[(A, B, C)] = {
			this.seq[(A, B, C)]({ case (a, _, _) => a })(a =>
				pb.seq[(A, B, C)]({ case (_, b, _) => b })(b =>
					pc.seq[(A, B, C)]({ case (_, _, c) => c })(c =>
						PU.lift(a, b, c))))
		}

		def quad[B, C, D](pb: PU[B], pc: PU[C], pd: PU[D]): PU[(A, B, C, D)] = {
			this.seq[(A, B, C, D)]({ case (a, _, _, _) => a })(a =>
				pb.seq[(A, B, C, D)]({ case (_, b, _, _) => b })(b =>
					pc.seq[(A, B, C, D)]({ case (_, _, c, _) => c })(c =>
						pd.seq[(A, B, C, D)]({ case (_, _, _, d) => d })(d =>
							PU.lift(a, b, c, d)))))
		}

		def wrap[B](f: A => B)(g: B => A): PU[B] = this.seq[B](g)(f andThen PU.lift)

		def fixedList(n: Int): PU[List[A]] = {
			n match {
				case 0 => PU.lift(List.empty)
				case x => this.pair(this.fixedList(x - 1)).wrap({ case (a, as) => a :: as })(as => (as.head, as.tail))
			}
		}
	}

	object PU {
		def lift[A](a: A): PU[A] = new PU(
			(_, state) => state,
			state => (a, state)
		)

		def unit: PU[Unit] = lift()
	}
}

trait StringPickler extends Pickler {

	override type State = String

	override def emptyState: State = ""

	def base: Int = 256

	def belowBase: PU[Int] = new PU(
		(a: Int, s: String) => a.toChar + s,
		(s: String) => (s.head.toInt, s.tail)
	)

	def zeroTo(i: Int): PU[Int] = {
		i match {
			case 0 => PU.lift(0)
			case n => zeroTo(n / base).pair(belowBase).wrap({ case (high, low) => high * base + low })(x => (x / base, x % base))
		}
	}

	def char: PU[Char] = zeroTo(255).wrap(_.toChar)(_.toInt)

	def boolean: PU[Boolean] = zeroTo(1).wrap({ case 0 => false; case 1 => true })({ case false => 0; case true => 1 })

	def natural: PU[Int] = {
		val half = base / 2
		belowBase.seq[Int](x => if (x < half) x else half + (x % half))(lo => if (lo < half) PU.lift(lo) else natural.wrap(hi => hi * half + lo)(n => (n / half) - 1))
	}

	def list[A](pa: PU[A]): PU[List[A]] = {
		natural.seq[List[A]](_.size)(pa.fixedList)
	}

	def string: PU[String] = {
		list(char).wrap(_.mkString)(_.toList)
	}

	def alt[A](pus: List[PU[A]])(f: A => Int): PU[A] = {
		zeroTo(pus.length - 1).seq(f)(pus(_))
	}

	def maybe[A](pa: PU[A]): PU[Option[A]] = {
		def tag[X](opt: Option[X]): Int = opt.map(_ => 1).getOrElse(0)

		alt(List(
			PU.lift(Option.empty[A]),
			pa.wrap[Option[A]](Option(_))(_.get)))(tag)
	}

	def either[A, B](pa: PU[A], pb: PU[B]): PU[Either[A, B]] = {
		def tag[X, Y](either: Either[X, Y]): Int = either.fold(_ => 0, _ => 1)
		def fromLeft[X, Y](either: Either[X, Y]): X = either.left.get
		def fromRight[X, Y](either: Either[X, Y]): Y = either.right.get

		alt(List(pa.wrap[Either[A, B]](Left(_))(fromLeft(_)), pb.wrap[Either[A, B]](Right(_))(fromRight(_))))(tag)
	}
}
