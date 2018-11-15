package catsbook.typeclasses

import catsbook.typeclasses.AnatomyOfATypeClass._

object InstanceSelection {

  sealed trait Shape
  case class Circle(radius: Double) extends Shape

  object covariance {
    val circles: List[Circle] = List(Circle(1.0), Circle(2.0), Circle(3.0))
    val shapes: List[Shape] = circles
  }

  object contravariance {
    trait JsonWriter[-A] {
      def write(value: A): Json
    }

    object JsonWriterInstances {
      implicit val stringWriter: JsonWriter[String] = JsString(_)

      implicit val numberWriter: JsonWriter[Number] = n => JsNumber(n.doubleValue())

      implicit def optionWriter[A](implicit
                                   writer: JsonWriter[A]): JsonWriter[Option[A]] = _.fold[Json](JsNull)(writer.write)
    }

    object Json {
      def toJson[A](value: A)(implicit writer: JsonWriter[A]): Json = writer.write(value)
    }

    import JsonWriterInstances._

    val shape: Shape = Circle(4.0)
    val circle: Circle = Circle(5.0)

    val circleWriter: JsonWriter[Circle] = circle => JsObject(Map(
      "circle" -> JsObject(Map(
        "radius" -> Json.toJson(Double.box(circle.radius))
      ))
    ))
    val shapeWriter: JsonWriter[Shape] = {
      case circle: Circle => circleWriter.write(circle)
      case other => throw new UnsupportedOperationException(s"$other cannot be converted to Json")
    }

    def format[A](value: A, writer: JsonWriter[A]): Json = writer.write(value)

    format(shape, shapeWriter)
    // format(shape, circleWriter) --> doesn't compile!
    format(circle, shapeWriter)
    format(circle, circleWriter)
  }
}
