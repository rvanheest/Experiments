import experiments.parsec.string.StringParser._

val itemParser1 = item
itemParser1.run("foo")

val itemParser2 = for {
	first <- item
	second <- item
	third <- item
} yield List(first, second, third).mkString
itemParser2.run("foo")

val digitParser = digit
digitParser.run("5")
digitParser.run("a")

val numberParser = number
numberParser.run("345")
numberParser.run("45a6")

val patternParser = for {
	s1 <- number
	s2 <- letter.atLeastOnce
	s3 <- number
} yield (s1, s2.mkString("[", "-", "]"), s3)
patternParser.run("45abc6")
