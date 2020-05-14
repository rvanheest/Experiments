package experiments.monadics.instances

import experiments.monadics.Monad

case class Identity[A](id: A)(implicit m: Monad[Identity]) {

  def map[B](f: A => B): Identity[B] = m.map(this)(f)

  def <*>[B, C](other: Identity[B])(implicit ev: A <:< (B => C)): Identity[C] = {
    m.<*>(this.map(ev), other)
  }

  def *>[B](other: Identity[B]): Identity[B] = m.*>(this, other)

  def <*[B](other: Identity[B]): Identity[A] = m.<*(this, other)

  def <**>[B](other: Identity[A => B]): Identity[B] = m.<**>(this, other)

  def flatMap[B](f: A => Identity[B]): Identity[B] = m.flatMap(this)(f)
  def >>=[B](f: A => Identity[B]): Identity[B] = flatMap(f)

  def andThen[B](other: Identity[B]): Identity[B] = m.andThen(this, other)
  def >>[B](other: Identity[B]): Identity[B] = andThen(other)

  def thenAnd[B](other: Identity[B]): Identity[A] = m.thenAnd(this, other)
  def <<[B](other: Identity[B]): Identity[A] = thenAnd(other)

  def flatten[B](implicit ev: A <:< Identity[B]): Identity[B] = m.flatten(this)(ev)
}
