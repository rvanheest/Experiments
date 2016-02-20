import experiments.parsec.StringParser._

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

val patternParser = for {
	s1 <- number
	s2 <- letter.atLeastOnce
	s3 <- number
} yield (s1, s2.mkString("[", "-", "]"), s3)
patternParser.parse("45abc6")
