package catsbook.typeclasses

object PrintableExercise extends App {

  trait Printable[A] {
    def format(a: A): String
  }

  object PrintableInstances {
    implicit val intPrintable: Printable[Int] = _.toString
    implicit val stringPrintable: Printable[String] = identity[String]
  }

  object Printable {
    def format[A](a: A)(implicit printable: Printable[A]): String = printable.format(a)

    def print[A](a: A)(implicit printable: Printable[A]): Unit = println(format(a))
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](val a: A) extends AnyVal {
      def format(implicit printable: Printable[A]): String = Printable.format(a)

      def print(implicit printable: Printable[A]): Unit = Printable.print(a)
    }
  }

  import PrintableInstances._
  import PrintableSyntax._

  case class Cat(name: String, age: Int, color: String)

  implicit def catPrintable: Printable[Cat] = cat => {
    val name = Printable.format(cat.name)
    val age = Printable.format(cat.age)
    val color = Printable.format(cat.color)

    s"$name is a $age year-old $color cat."
  }

  val cat = Cat("Alice", 2, "black")
  Printable.print(cat)
  cat.print
}
