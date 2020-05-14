package experiments.codingChallenges

object Sorting extends App {

  def merge(as: Array[Int], bs: Array[Int], n: Int, m: Int): Unit = {
    var k = n + m - 1
    var i = n - 1
    var j = m - 1

    while (j >= 0 && i >= 0) {
      val a = as(i)
      val b = bs(j)

      if (a > b) {
        as(k) = a
        i -= 1
      }
      else {
        as(k) = b
        j -= 1
      }
      k -= 1
    }

    while (j >= 0) {
      as(k) = bs(j)
      j -= 1
      k -= 1
    }
  }

  def example1(): Unit = {
    val as = {
      val as = Array.fill(7)(0)
      as(0) = 1
      as(1) = 5
      as(2) = 9

      as
    }
    val bs = Array(0, 2)
    val n = 3
    val m = 2

    println(s"merge ${as.mkString("[", ", ", "]")} with ${bs.mkString("[", ", ", "]")}")

    merge(as, bs, n, m)
    println(as.mkString("[", ", ", "]"))
  }

  def example2(): Unit = {
    val as = {
      val as = Array.fill(7)(0)
      as(0) = 0
      as(1) = 2
      as(2) = 4

      as
    }
    val bs = Array(6, 7)
    val n = 3
    val m = 2

    println(s"merge ${as.mkString("[", ", ", "]")} with ${bs.mkString("[", ", ", "]")}")

    merge(as, bs, n, m)
    println(as.mkString("[", ", ", "]"))
  }

  println("Example 1")
  example1()

  println
  println("Example 2")
  example2()
}
