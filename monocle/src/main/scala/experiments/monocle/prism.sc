import monocle.Prism
import monocle.std.double.doubleToInt

sealed trait Json
case object JNull extends Json
case class JStr(v: String) extends Json
case class JNum(v: Double) extends Json
case class JObj(v: Map[String, Json]) extends Json

val jsonDouble = Prism[Json, Double] {
  case JNum(d) => Some(d)
  case _ => None
}(JNum)
val jsonString = Prism.partial[Json, String]{ case JStr(s) => s }(JStr)

jsonString("hello")
jsonString.getOption(JStr("hello"))
jsonString.getOption(JNum(0))

jsonString.set("foo")(JStr("hello"))
jsonString.set("bar")(JNum(0))

jsonString.modify(_.reverse)(JStr("hello"))
jsonString.modify(_.reverse)(JNum(0))

jsonString.modifyOption(_.reverse)(JStr("hello"))
jsonString.modifyOption(_.reverse)(JNum(0))

val jsonInt = jsonDouble.composePrism(doubleToInt)
jsonInt(5)
jsonInt.getOption(JStr("hello"))
jsonInt.getOption(JNum(1))
jsonInt.getOption(JNum(1.1))
