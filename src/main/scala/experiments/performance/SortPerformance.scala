package experiments.performance

object SortPerformance extends App with Measurement {
  
  val n = 10000000
  val stream = Stream.continually(math.random).take(n)
  val list = stream.toList
  
  timed("list.sorted") {
    list.sorted
  }
  
  timed("stream.sorted") {
    stream.sorted
  }
}
