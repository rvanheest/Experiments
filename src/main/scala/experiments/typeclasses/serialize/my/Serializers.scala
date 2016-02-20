package experiments.typeclasses.serialize.my

import scala.language.reflectiveCalls

trait Serializable[T] {
	def serialize(t: T): String
}

object Serializers {
	implicit def addSerializable[T : Serializable](t: T): Object {def serialize: String} = {
		new {
			def serialize: String = implicitly[Serializable[T]].serialize(t)
		}
	}

	implicit def exprIsSerializable = {
		new Serializable[Expression] {
			def serialize(expr: Expression): String = {
				expr match {
					case Number(value) => value.toString
					// can't use lhs.serialize here, since Serializable[Expression] doesn't exist yet at this point
					case Plus(lhs, rhs) => s"${serialize(lhs)} + ${serialize(rhs)}"
					case Minus(lhs, rhs) => s"${serialize(lhs)} - ${serialize(rhs)}"
				}
			}
		}
	}

	implicit def listOfExprIsSerializable = {
		new Serializable[List[Expression]] {
			override def serialize(exprs: List[Expression]): String = {
				exprs.map(_.serialize).mkString("List(", ", ", ")")
			}
		}
	}
}
