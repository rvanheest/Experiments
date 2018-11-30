package catsbook.monad

import cats.data.Reader

object CatsReaderMonad extends App {

  case class Cat(name: String, favoriteFood: String)
  val cat = Cat("Tom", "Jerry")

  val catName: Reader[Cat, String] = Reader(_.name)
  println(catName.run(cat)) // Tom

  val greetCat = catName.map(name => s"Hello $name")
  println(greetCat.run(cat)) // Hello Tom

  val feedCat: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed = for {
    greet <- greetCat
    feed <- feedCat
  } yield s"$greet. $feed."

  println(greetAndFeed(cat)) // Hello Tom. Have a nice bowl of Jerry.
}
