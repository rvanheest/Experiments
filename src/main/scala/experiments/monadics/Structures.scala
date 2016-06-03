package experiments.monadics

import experiments.monadics.instances.{Just, Maybe, None}

import scala.language.{higherKinds, reflectiveCalls}

trait Semigroup[S] {
  def append(a1: S, a2: S): S
}

trait Monoid[M] extends Semigroup[M] {
  def empty: M
}

trait Foldable[F[_]] {
  def fold[M](fM: F[M])(implicit ev: M <:< Monoid[M]): M

  def foldMap[A, M](fA: F[A])(f: A => M)(implicit ev: M <:< Monoid[M]): M
}

trait Functor[F[_]] {
  def map[A, B](functor: F[A])(f: A => B): F[B]
}

trait Applicative[App[_]] extends Functor[App] {
  def create[A](a: A): App[A]

  def <*>[A, B](appFunc: App[A => B], appA: App[A]): App[B]

  def *>[A, B](appA: App[A], appB: App[B]): App[B] = {
    <*>(map[A, B => B](appA)(_ => identity[B]), appB)
  }

  def <*[A, B](appA: App[A], appB: App[B]): App[A] = {
    <*>(map[A, B => A](appA)(a => _ => a), appB)
  }

  def <**>[A, B](appA: App[A], appFunc: App[A => B]): App[B] = {
    <*>(appFunc, appA)
  }
}

trait Monad[M[_]] extends Applicative[M] {
  override def <*>[A, B](mFunc: M[A => B], mA: M[A]): M[B] = {
    flatMap(mA)(a => map(mFunc)(_(a)))
  }

  override def *>[A, B](mA: M[A], mB: M[B]): M[B] = {
    andThen(mA, mB)
  }

  override def <*[A, B](mA: M[A], mB: M[B]): M[A] = {
    thenAnd(mA, mB)
  }

  def flatMap[A, B](monad: M[A])(f: A => M[B]): M[B]

  def andThen[A, B](mA: M[A], mB: M[B]): M[B] = {
    flatMap(mA)(a => mB)
  }

  def thenAnd[A, B](mA: M[A], mB: M[B]): M[A] = {
    flatMap(mA)(a => map(mB)(_ => a))
  }

  def flatten[A, B](mA: M[A])(implicit ev: A <:< M[B]): M[B] = {
    flatMap(mA)(ev)
  }
}

trait Alternative[Alt[_]] extends Applicative[Alt] {
  def empty[A]: Alt[A]

  // TODO this method needs to go to another type class, maybe Foldable???
  def getOrElse[A, B >: A](alt: Alt[A], default: => B): B

  def orElse[A, B >: A](alt1: Alt[A], alt2: => Alt[B]): Alt[B]

  def atLeastOnce[A](alt: Alt[A]): Alt[List[A]] = {
    atLeastOnce_v(alt)
  }

  def many[A](alt: Alt[A]): Alt[List[A]] = {
    many_v(alt)
  }

  private def many_v[A](alt: Alt[A]): Alt[List[A]] = {
    orElse(atLeastOnce_v(alt), create(Nil))
  }

  private def atLeastOnce_v[A](alt: Alt[A]): Alt[List[A]] = {
    <*>(map[A, List[A] => List[A]](alt)(a => a :: _), many_v(alt))
  }

  def maybe[A](alt: Alt[A]): Alt[Maybe[A]] = {
    orElse(map(alt)(Just(_)), create(None))
  }
}

trait MonadPlus[MP[_]] extends Monad[MP] with Alternative[MP] {
  def mplus[A, B >: A](mp1: MP[A], mp2: MP[B]): MP[B] = {
    orElse(mp1, mp2)
  }

  def filter[A](mp: MP[A])(predicate: A => Boolean): MP[A] = {
    flatMap(mp)(a => if (predicate(a)) create(a) else empty[A])
  }

  def filterNot[A](mp: MP[A])(predicate: A => Boolean): MP[A] = {
    filter(mp)(!predicate(_))
  }

  def takeUntil[A](mp: MP[A])(predicate: A => Boolean): MP[List[A]] = {
    many(filterNot(mp)(predicate))
  }

  def takeWhile[A](mp: MP[A])(predicate: A => Boolean): MP[List[A]] = {
    many(filter(mp)(predicate))
  }
}

// TODO MonadTrans

trait Category[Cat[_, _]] {
  def id[A]: Cat[A, A]

  def compose[A, B, C](bc: Cat[B, C], ab: Cat[A, B]): Cat[A, C]

  def >>>[A, B, C](ab: Cat[A, B], bc: Cat[B, C]): Cat[A, C] = compose(bc, ab)
}

trait Arrow[Arr[_, _]] extends Category[Arr] {

  def create[A, B](f: A => B): Arr[A, B]

  def first[A, B, C](arr: Arr[A, B]): Arr[(A, C), (B, C)]

  def second[A, B, C](arr: Arr[A, B]): Arr[(C, A), (C, B)] = {
    val f = first[A, B, C](arr)
    val swap1 = create[(C, A), (A, C)](_.swap)
    val swap2 = create[(B, C), (C, B)](_.swap)

    >>>(>>>(swap1, f), swap2)
  }

  def ***[A, B, C, D](arr1: Arr[A, B], arr2: Arr[C, D]): Arr[(A, C), (B, D)] = {
    val a1 = first[A, B, C](arr1)
    val a2 = second[C, D, B](arr2)

    >>>(a1, a2)
  }

  def &&&[A, B, C](arr1: Arr[A, B], arr2: Arr[A, C]): Arr[A, (B, C)] = {
    val f = create[A, (A, A)](a => (a, a))
    val g = ***(arr1, arr2)

    >>>(f, g)
  }

  def liftA2[A, B, C, D](arr1: Arr[A, B], arr2: Arr[A, C])(f: (B, C) => D): Arr[A, D] = {
    val g = &&&(arr1, arr2)

    >>>(g, create(f.tupled))
  }
}

trait ArrowApply[Arr[_, _]] extends Arrow[Arr] {
  def app[A, B]: Arr[(Arr[A, B], A), B]

  def app2[B, C]: Arr[B, Arr[Arr[B, C], C]]
}

trait ArrowLoop[Arr[_, _]] extends Arrow[Arr] {
  def loop[B, C, D](arr: Arr[(B, D), (C, D)]): Arr[B, C]
}

trait ArrowApplicative[A, Arr[_, _]] extends Arrow[Arr] with Applicative[({type s[x] = Arr[A, x]})#s] {
  def map[B, C](arr: Arr[A, B])(f: B => C): Arr[A, C] = {
    >>>(arr, create(f))
  }

  def <*>[B, C](arrF: Arr[A, B => C], arr: Arr[A, B]): Arr[A, C] = {
    liftA2(arrF, arr)((f, b) => f(b))
  }
}
