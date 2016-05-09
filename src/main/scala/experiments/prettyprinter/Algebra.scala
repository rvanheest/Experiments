package experiments.prettyprinter

import scala.language.implicitConversions

sealed abstract class Doc {
  import experiments.prettyprinter.Doc._

  def layout: String
  def ~(that: Doc) = Cons(this, that)
  def ~~(that: Doc) = this ~ space ~ that
  def <~(that: Doc) = this ~ line ~ that
  def ~>(that: Doc) = this ~ nest(2, line ~ that)
}
object Doc {
  implicit def string(s: String): Doc = Text(s)
  val line  = Line
  val space = Text(" ")
  def nest(n: Int, d: Doc) = Nest(n, d)
  def block(d: Doc): Doc = space ~ "{" ~> d <~ "}"
}

case object Empty extends Doc {
  def layout = ""
}

case object Line extends Doc {
  def layout = "\n"
}

case class Text(s: String) extends Doc {
  def layout = s
}

case class Nest(n: Int, d: Doc) extends Doc {
  def layout = d match {
    case e@Empty => e.layout
    case Line => "\n" + (" " * n)
    case s@Text(_) => s.layout
    case Cons(l, r) => Cons(Nest(n, l), Nest(n, r)).layout
    case Nest(m, x) => Nest(n + m, x).layout
  }
}

case class Cons(left: Doc, right: Doc) extends Doc {
  def layout = left.layout + right.layout
}
