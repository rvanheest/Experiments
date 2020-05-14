package experiments.cats.exercises

import cats._

object functor extends App {

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }

  implicit def function1Functor[In]: Functor[In => ?] = new Functor[In => ?] {
    def map[A, B](fa: In => A)(f: A => B): Function1[In, B] = fa andThen f
  }

  println(Functor[List].map(List("qwer", "adsfg"))(_.length))
  println

  println(List("qwer", "adsfg").map(_.length))
  println

  println(Functor[Option].map(Option("Hello"))(_.length))
  Functor[Option].map(Option.empty[String])(_.length)
  println

  println(Option("Hello").map(_.length))
  println(Option.empty[String].map(_.length))
  println

  val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
  println(lenOption(Some("abcd")))
  println(lenOption(Some("Hello")))
  println

  val source = List("Cats", "is", "awesome")
  val product = Functor[List].fproduct(source)(_.length).toMap
  println(product.getOrElse("Cats", 0))
  println(product.getOrElse("is", 0))
  println(product.getOrElse("is", 0))
  println

  val listOpt = Functor[List] compose Functor[Option]
  println(listOpt.map(List(Some(1), None, Some(3)))(_ + 1))
}
