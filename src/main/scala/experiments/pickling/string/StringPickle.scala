package experiments.pickling.string

import experiments.parsec.string.StringParser
import experiments.pickling.{ Pickle, PickleBuilder }

import scala.language.postfixOps
import scala.util.Try

case class StringPickle[A](override val pickle: (A, String) => Try[String],
                           override val unpickle: String => (Try[A], String))
  extends Pickle[A, String](pickle, unpickle) {

  type Repr[X] = StringPickle[X]

  protected[this] implicit def builder[X]: PickleBuilder[X, String, StringPickle[X]] = StringPickle.stringPickleBuilder
}

object StringPickle {
  protected[StringPickle] implicit def stringPickleBuilder[X]: PickleBuilder[X, String, StringPickle[X]] = {
    new PickleBuilder[X, String, StringPickle[X]] {
      def apply(pickle: (X, String) => Try[String], unpickle: String => (Try[X], String)): StringPickle[X] = {
        StringPickle(pickle, unpickle)
      }
    }
  }

  def item: StringPickle[Char] = {
    StringPickle(
      pickle = (c, s) => Try(c +: s),
      unpickle = StringParser.item.run
    )
  }

  def digit: StringPickle[Char] = item.satisfy(_.isDigit)

  def number: StringPickle[String] = digit.atLeastOnce.seq[String](_.toList).map(_.mkString)

  def lower: StringPickle[Char] = item.satisfy(_.isLower)

  def upper: StringPickle[Char] = item.satisfy(_.isUpper)

  def letter: StringPickle[Char] = item.satisfy(_.isLetter)

  def alphanum: StringPickle[Char] = item.satisfy(c => c.isLetter || c.isDigit)

  def char(c: Char): StringPickle[Char] = item.satisfy(c ==)

  def space: StringPickle[Char] = char(' ')

  def string(s: String): StringPickle[String] = {
    StringPickle(
      pickle = (str, state) =>
        s.toList match {
          case x :: xs => (for {
            _ <- char(x).seq[String](_.head)
            _ <- string(xs.mkString).seq[String](_.tail)
          } yield s).pickle(str, state)
          case Nil => Try(state)
        },
      unpickle = StringParser.string(s).run
    )
  }
}
