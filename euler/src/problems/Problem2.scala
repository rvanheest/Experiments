package problems

object Problem2 extends App {

  println {
    Stream.continually(0)
      .scanLeft((0, 1)) { case ((l, r), _) => (r, l + r) }
      .map { case (_, r) => r }
      .drop(1)
      .filter(_ % 2 == 0)
      .takeWhile(_ < 4000000)
      .sum
  }
}
