package experiments.hlist

import scala.annotation.tailrec

sealed trait HList {
  def :+:[V](v: V): HList
}

final class HNil extends HList {
  def :+:[V](v: V): HCons[V, HNil] = HCons(v, this)

  override def toString: String = "()"
}

final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  def :+:[V](v: V): HCons[V, HCons[H, T]] = HCons(v, this)

  override def toString: String = {
    @tailrec
    def rec(hlist: HList, result: List[Any] = List.empty): List[Any] = {
      hlist match {
        case _: HNil => result
        case HCons(h, t) => rec(t, result :+ h)
      }
    }

    rec(this).mkString("(", ", ", ")")
  }
}
