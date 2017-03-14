package experiments.pickling.xml

case class Pickle[A, State](pickle: (A, State) => State) {

	def maybe: Pickle[Option[A], State] = {
		Pickle((optS: Option[A], state: State) => optS.map(x => this.pickle(x, state)).getOrElse(state))
	}

	def seq[B](f: B => A)(g: A => Pickle[B, State]): Pickle[B, State] = {
		Pickle[B, State]((b: B, s: State) => {
			val t = f(b)
			this.pickle(t, g(t).pickle(b, s))
		})
	}

	def pair[B](pb: Pickle[B, State]): Pickle[(A, B), State] = {
		this.seq[(A, B)]({ case (a, _) => a })(a =>
			pb.seq[(A, B)]({ case (_, b) => b })(b =>
				Pickle.lift(a, b)))
	}

	def triple[B, C](pb: Pickle[B, State], pc: Pickle[C, State]): Pickle[(A, B, C), State] = {
		this.seq[(A, B, C)]({ case (a, _, _) => a })(a =>
			pb.seq[(A, B, C)]({ case (_, b, _) => b })(b =>
				pc.seq[(A, B, C)]({ case (_, _, c) => c })(c =>
					Pickle.lift(a, b, c))))
	}

	def quad[B, C, D](pb: Pickle[B, State], pc: Pickle[C, State], pd: Pickle[D, State]): Pickle[(A, B, C, D), State] = {
		this.seq[(A, B, C, D)]({ case (a, _, _, _) => a })(a =>
			pb.seq[(A, B, C, D)]({ case (_, b, _, _) => b })(b =>
				pc.seq[(A, B, C, D)]({ case (_, _, c, _) => c })(c =>
					pd.seq[(A, B, C, D)]({ case (_, _, _, d) => d })(d =>
						Pickle.lift(a, b, c, d)))))
	}

	def wrap[B](f: A => B): WrapBuilder[B] = {
		new WrapBuilder(this, f)
	}

	class WrapBuilder[B](pickle: Pickle[A, State], f: A => B) {
		def unwrap(g: PartialFunction[B, A]): Pickle[B, State] = {
			pickle.seq[B](b => if (g isDefinedAt b) g(b) else sys.error("undefined"))(t => Pickle.lift[B, State](f(t)))
		}
	}
}

object Pickle {
	def lift[A, State](a: A): Pickle[A, State] = new Pickle[A, State]((_, s) => s)

	def alt[A, State](as: Array[Pickle[A, State]])(selector: A => Int): Pickle[A, State] = {
		Pickle[A, State]((a, state) => as(selector(a)).pickle(a, state))
	}
}
