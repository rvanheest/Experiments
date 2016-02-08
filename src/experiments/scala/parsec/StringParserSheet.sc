import experiments.scala.parsec.StringParser._

val itemParser1 = item
itemParser1.parse("foo")

val itemParser2 = for {
	first <- item
	second <- item
	third <- item
} yield List(first, second, third).mkString
itemParser2.parse("foo")

val digitParser = digit
digitParser.parse("5")
digitParser.parse("a")

val numberParser = number
numberParser.parse("345")
numberParser.parse("45a6")
