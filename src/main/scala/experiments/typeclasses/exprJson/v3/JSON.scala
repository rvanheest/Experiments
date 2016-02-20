package experiments.typeclasses.exprJson.v3

sealed trait JsonValue
case class JsonObject(entries: Map[String, JsonValue]) extends JsonValue
case class JsonArray(entries: Seq[JsonValue]) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonNumber(value: BigDecimal) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case object JsonNull extends JsonValue

object JsonWriter {
	def write(value: JsonValue): String = {
		value match {
			case JsonObject(entries) =>
				val serializedEntries = for ((key, value) <- entries) yield key + ": " + write(value)
				s"{ ${serializedEntries.mkString(", ")} }"
			case JsonArray(entries) =>
				val serializedEntries = entries.map(write)
				s"[ ${serializedEntries.mkString(",")} ]"
			case JsonString(s) => "\"" + s + "\""
			case JsonNumber(n) => n.toString
			case JsonBoolean(b) => b.toString
			case JsonNull => "null"
		}
	}

	def write[A: Json](value: A): String = {
		write(implicitly[Json[A]].json(value))
	}
}

trait Json[A] {
	def json(value: A): JsonValue
}
object Json {
	implicit val intJson = new Json[Int] {
		override def json(n: Int): JsonValue = JsonNumber(n)
	}

	implicit def pairJson[T1: Json, T2: Json] = new Json[(T1, T2)] {
		override def json(pair: (T1, T2)): JsonValue = JsonObject(
			Map("fst" -> implicitly[Json[T1]].json(pair._1),
				"snd" -> implicitly[Json[T2]].json(pair._2)))
	}
}
