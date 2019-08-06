package experiments.caching

import scalacache._
import scalacache.guava._
import scalacache.modes.try_._

import scala.util.Try

object DefaultGuavaCaching extends App {

  case class Cat(id: Int, name: String, colour: String)
  val cat1 = Cat(1, "eric", "red")
  val cat2 = Cat(2, "mies", "black")

  implicit val guavaCache: Cache[Cat] = GuavaCache[Cat]

  println("put values")
  put("eric")(cat1)
  put("mies")(cat2)

  println("get values")
  get("mies").foreach(println) // mies should be in the cache
  get("eric").foreach(println) // eric should be in the cache
  get("unknown").foreach(println) // unknown should NOT be in the cache

  caching("tom")(None) {
    // some computation
    Cat(3, "tom", "yellow")
  }.foreach(println)

  cachingF("henk")(None) {
    // some computation wrapped in a monad
    Try {
      Cat(4, "henk", "green")
    }
  }.foreach(println)
  
  caching("tom")(None) {
    throw new Exception("not supposed to end up here")
  }.foreach(println) // should get the value from the cache and not calculate it
}
