package experiments.pickling.xml

trait Pickle[T, State] { self =>
	def pickle(t: T, state: State): State

	def maybe[S](implicit ev: S =:= T): Pickle[Option[S], State] = new Pickle[Option[S], State] {
		override def pickle(optS: Option[S], state: State): State = {
			optS match {
				case Some(x) => self.pickle(ev(x), state)
				case None => state
			}
		}
	}

	def seq[R](f: R => T)(g: T => Pickle[R, State]): Pickle[R, State] = new Pickle[R, State] {
		override def pickle(r: R, s: State): State = {
			val t = f(r)
			self.pickle(t, g(t).pickle(r, s))
		}
	}

	def pair[B](pb: Pickle[B, State]): Pickle[(T, B), State] = {
		this.seq[(T, B)]({ case (t, _) => t })(t =>
			pb.seq[(T, B)]({ case (_, b) => b })(b =>
				Pickle.lift(t, b)))
	}

	def triple[B, C](pb: Pickle[B, State], pc: Pickle[C, State]): Pickle[(T, B, C), State] = {
		this.seq[(T, B, C)]({ case (t, _, _) => t })(t =>
			pb.seq[(T, B, C)]({ case (_, b, _) => b })(b =>
				pc.seq[(T, B, C)]({ case (_, _, c) => c })(c =>
					Pickle.lift(t, b, c))))
	}

	def quad[B, C, D](pb: Pickle[B, State], pc: Pickle[C, State], pd: Pickle[D, State]): Pickle[(T, B, C, D), State] = {
		this.seq[(T, B, C, D)]({ case (t, _, _, _) => t })(t =>
			pb.seq[(T, B, C, D)]({ case (_, b, _, _) => b })(b =>
				pc.seq[(T, B, C, D)]({ case (_, _, c, _) => c })(c =>
					pd.seq[(T, B, C, D)]({ case (_, _, _, d) => d })(d =>
						Pickle.lift(t, b, c, d)))))
	}

	def wrap[B](f: T => B)(g: PartialFunction[B, T]): Pickle[B, State] = {
		this.seq[B](b => if (g.isDefinedAt(b)) g(b) else sys.error("undefined"))(t => Pickle.lift(f(t)))
	}
}

object Pickle {
	def lift[A, State](a: A): Pickle[A, State] = new Pickle[A, State] {
		override def pickle(a: A, s: State): State = s
	}

	def alt[A, State](as: Array[Pickle[A, State]])(selector: A => Int): Pickle[A, State] = new Pickle[A, State] {
		override def pickle(a: A, state: State): State = {
			val index = selector(a)
			val p = as(index)
			p.pickle(a, state)
		}
	}
}
