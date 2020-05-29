package problems

object Problem1 extends App {

  println {
    Stream.range(0, 1000)
      .filter(i => i % 3 == 0 || i % 5 == 0)
      .sum
  }
}
