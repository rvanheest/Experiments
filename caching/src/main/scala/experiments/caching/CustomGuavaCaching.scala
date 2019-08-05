package experiments.caching

import java.util.concurrent.TimeUnit

import com.google.common.cache.CacheBuilder
import scalacache._
import scalacache.modes.try_._
import scalacache.guava._

object CustomGuavaCaching extends App {

  case class Cat(id: Int, name: String, colour: String)
  val cat1 = Cat(1, "eric", "red")
  val cat2 = Cat(2, "mies", "black")

  val underlying = CacheBuilder.newBuilder()
    .expireAfterAccess(10L, TimeUnit.SECONDS)
    .build[String, Entry[Cat]]
  implicit val guavaCache: Cache[Cat] = GuavaCache(underlying)

  println("put values")
  put("eric")(cat1)
  put("mies")(cat2)

  println("get immediately")
  get("mies").foreach(println) // mies should be in the cache
  
  Thread.sleep(5 * 1000)

  println("get after 5 seconds...")
  get("eric").foreach(println) // eric should be in the cache
  get("unknown").foreach(println) // unknown should NOT be in the cache
  
  Thread.sleep(6 * 1000)

  println("get after 6 seconds...")
  get("eric").foreach(println) // eric should still be in the cache
  get("mies").foreach(println) // mies should no longer be in the cache

  caching("mies")(None) {
    // some computation
    cat2
  }.foreach(println) // add mies back in the cache
}
