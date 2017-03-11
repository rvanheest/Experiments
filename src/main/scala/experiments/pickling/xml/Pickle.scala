package experiments.pickling.xml

import scala.xml.Node

trait Pickle[T] { self =>
	def pickle(t: T, xml: Node): Node

	def maybe[S](implicit ev: S =:= T): Pickle[Option[S]] = new Pickle[Option[S]] {
		override def pickle(s: Option[S], xml: Node): Node = {
			s match {
				case Some(x) => self.pickle(x, xml)
				case None => xml
			}
		}
	}

	def seq[R](f: R => T)(g: T => Pickle[R]): Pickle[R] = new Pickle[R] {
		override def pickle(r: R, s: Node): Node = {
			val t = f(r)
			self.pickle(t, g(t).pickle(r, s))
		}
	}

	def pair[B](pb: Pickle[B]): Pickle[(T, B)] = {
		this.seq[(T, B)]({ case (t, _) => t })(t =>
			pb.seq[(T, B)]({ case (_, b) => b })(b =>
				Pickle.lift(t, b)))
	}

	def triple[B, C](pb: Pickle[B], pc: Pickle[C]): Pickle[(T, B, C)] = {
		this.seq[(T, B, C)]({ case (t, _, _) => t })(t =>
			pb.seq[(T, B, C)]({ case (_, b, _) => b })(b =>
				pc.seq[(T, B, C)]({ case (_, _, c) => c })(c =>
					Pickle.lift(t, b, c))))
	}

	def quad[B, C, D](pb: Pickle[B], pc: Pickle[C], pd: Pickle[D]): Pickle[(T, B, C, D)] = {
		this.seq[(T, B, C, D)]({ case (t, _, _, _) => t })(t =>
			pb.seq[(T, B, C, D)]({ case (_, b, _, _) => b })(b =>
				pc.seq[(T, B, C, D)]({ case (_, _, c, _) => c })(c =>
					pd.seq[(T, B, C, D)]({ case (_, _, _, d) => d })(d =>
						Pickle.lift(t, b, c, d)))))
	}

	def wrap[B](f: T => B)(g: PartialFunction[B, T]): Pickle[B] = {
		this.seq[B](b => if (g.isDefinedAt(b)) g(b) else sys.error("undefined"))(t => Pickle.lift(f(t)))
	}
}

object Pickle {
	def lift[A](a: A): Pickle[A] = new Pickle[A] {
		override def pickle(a: A, s: Node): Node = s
	}

	def alt[A](as: Array[Pickle[A]])(selector: A => Int): Pickle[A] = new Pickle[A] {
		override def pickle(a: A, xml: Node): Node = {
			val index = selector(a)
			val p = as(index)
			p.pickle(a, xml)
		}
	}
}
