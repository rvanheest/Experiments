package catsbook.typeclasses

object AnatomyOfATypeClass extends App {

  // domain objects
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  case object JsNull extends Json

  case class Person(name: String, email: String, age: Int)

  // type class for Json serialization
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  // type class instances
  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] = JsString(_)

    implicit val numberWriter: JsonWriter[Number] = n => JsNumber(n.doubleValue())

    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = _.fold[Json](JsNull)(writer.write)

    implicit val personWriter: JsonWriter[Person] = person => JsObject(Map(
      "name" -> stringWriter.write(person.name),
      "email" -> stringWriter.write(person.email),
      "age" -> numberWriter.write(person.age),
    ))
  }

  // type class interface
  object Json {
    def toJson[A](value: A)(implicit writer: JsonWriter[A]): Json = writer.write(value)
  }

  // interface syntax
  object JsonSyntax {
    implicit class JsonWriterOps[A](val value: A) extends AnyVal {
      def toJson(implicit writer: JsonWriter[A]): Json = Json.toJson(value)
    }
  }

  // examples

  import JsonWriterInstances._
  import JsonSyntax._

  private val person = Person("Jim Jones", "jim@jones.com", 34)
  println(Json.toJson(person))
  println(person.toJson)
  println(Option(person).toJson)
  println(Option.empty[Person].toJson)
}
