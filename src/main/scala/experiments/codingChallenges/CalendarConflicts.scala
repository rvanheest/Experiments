package experiments.codingChallenges

import scala.collection.mutable

object CalendarConflicts extends App {

  case class Event(startTime: Int, endTime: Int, activity: String) {
    require(endTime > startTime, "endTime must be greater than startTime")
  }

  val calendar = List(
    Event(1, 2, "a"),
    Event(3, 5, "b"), // conflict b/c
    Event(4, 6, "c"),
    Event(7, 10, "d"), // conflict d/e
    //                                    becomes conflict d/e/f
    Event(8, 11, "e"), // conflict e/f
    Event(10, 12, "f"),
    Event(13, 14, "g"), // conflict g/h
    Event(13, 14, "h")
  )

  println(findConflicts(calendar).map(_.activity))

  def findConflicts(calendar: List[Event]): List[Event] = {
    case class Helper(time: Int, start: Boolean, event: Event)
    val allConflicts = Set.newBuilder[Event]
    val currentConflicts = mutable.Set.empty[Event]

    val (result, _) = calendar
      .flatMap(event => List(
        Helper(event.startTime, start = true, event),
        Helper(event.endTime, start = false, event)
      ))
      .sortBy(_.time)
      .foldLeft((allConflicts, currentConflicts)) {
        case ((all, current), Helper(_, true, event)) =>
          current += event

          if (current.size > 1)
            all ++= current

          (all, current)
        case ((all, current), Helper(_, false, event)) =>
          current -= event

          (all, current)
      }

    result.result().toList.sortBy(_.startTime)
  }
}
