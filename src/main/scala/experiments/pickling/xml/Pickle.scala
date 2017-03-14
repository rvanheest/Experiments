package experiments.pickling.xml

case class Pickle[A, State](pickle: (A, State) => State) {

	def maybe: Pickle[Option[A], State] = {
		Pickle((optA: Option[A], state: State) => optA.map(x => this.pickle(x, state)).getOrElse(state))
	}

	def seq[B](f: B => A): SeqBuilder[B] = new SeqBuilder(this, f)

	def pair[B](pb: Pickle[B, State]): Pickle[(A, B), State] = {
		for {
			a <- this.seq[(A, B)]({ case (a, _) => a })
			b <- pb.seq[(A, B)]({ case (_, b) => b })
		} yield (a, b)
	}

	def triple[B, C](pb: Pickle[B, State], pc: Pickle[C, State]): Pickle[(A, B, C), State] = {
		for {
			a <- this.seq[(A, B, C)]({ case (a, _, _) => a })
			b <- pb.seq[(A, B, C)]({ case (_, b, _) => b })
			c <- pc.seq[(A, B, C)]({ case (_, _, c) => c })
		} yield (a, b, c)
	}

	def quad[B, C, D](pb: Pickle[B, State], pc: Pickle[C, State], pd: Pickle[D, State]): Pickle[(A, B, C, D), State] = {
		for {
			a <- this.seq[(A, B, C, D)]({ case (a, _, _, _) => a })
			b <- pb.seq[(A, B, C, D)]({ case (_, b, _, _) => b })
			c <- pc.seq[(A, B, C, D)]({ case (_, _, c, _) => c })
			d <- pd.seq[(A, B, C, D)]({ case (_, _, _, d) => d })
		} yield (a, b, c, d)
	}

	def wrap[B](f: A => B): WrapBuilder[B] = new WrapBuilder(this, f)

	class SeqBuilder[B](pickle: Pickle[A, State], f: B => A) {
		def map(g: A => B): Pickle[B, State] = {
			Pickle((b, state) => pickle.pickle(f(b), state))
		}

		def flatMap(g: A => Pickle[B, State]): Pickle[B, State] = {
			Pickle((b, state) => {
				val a = f(b)
				pickle.pickle(a, g(a).pickle(b, state))
			})
		}
	}

	class WrapBuilder[B](pickle: Pickle[A, State], f: A => B) {
		def unwrap(g: PartialFunction[B, A]): Pickle[B, State] = {
			pickle.seq[B](b => if (g isDefinedAt b) g(b) else sys.error("undefined")).flatMap(t => Pickle.lift[B, State](f(t)))
		}
	}
}

object Pickle {
	def lift[A, State](a: A): Pickle[A, State] = Pickle[A, State]((_, s) => s)

	def alt[A, State](as: Array[Pickle[A, State]])(selector: A => Int): Pickle[A, State] = {
		Pickle[A, State]((a, state) => as(selector(a)).pickle(a, state))
	}
}
