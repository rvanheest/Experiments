package experiments.performance

import scala.collection.mutable.ListBuffer

object AppendPerformance extends App with Measurement {
  
  val list1 = (1 to 2000000).to[ListBuffer]
  val list2 = (2000001 to 4000000).to[ListBuffer]
  val list3 = (4000001 to 6000000).to[ListBuffer]

  timed("task 1") {
    (list1 ++ list2) ++ list3
  }

  timed("task 2") {
    list1 ++ (list2 ++ list3)
  }

  timed("task 3") {
    list1 ++ list2 ++ list3
  }
}
