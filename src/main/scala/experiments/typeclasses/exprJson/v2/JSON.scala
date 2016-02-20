package experiments.typeclasses.exprJson.v2

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

	def write[A](value: A)(implicit conv: JsonConverter[A]): String = write(conv.convertToJson(value))
}

trait JsonConverter[A] {
	def convertToJson(value: A): JsonValue
}
