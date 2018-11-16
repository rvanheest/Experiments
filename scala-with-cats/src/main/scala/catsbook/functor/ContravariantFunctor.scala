package catsbook.functor

object ContravariantFunctor extends App {

  trait Printable[A] {
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] = value => format(func(value))
  }
  object Printable {
    def apply[A](implicit printable: Printable[A]): Printable[A] = printable
  }
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  implicit val stringPrintable: Printable[String] = "\"" + _ + "\""

  implicit val booleanPrintable: Printable[Boolean] = if (_) "yes"
                                                      else "no"

  println(format("hello")) // "hello"
  println(format(true)) // yes
  println(format(false)) // no

  case class Box[A](value: A)

  implicit def boxPrintable[A: Printable]: Printable[Box[A]] = Printable[A].contramap[Box[A]](_.value)

  println(format(Box("hello world"))) // "hello world"
  println(format(Box(true))) // yes
}
