import experiments.pickling.string.StringPickle._

val itemPickle1 = item
itemPickle1.unpickle.run("foo")
itemPickle1.pickle('f', "oo")

val itemPickle2 = for {
  first <- item.seq[String](s => s(0))
  second <- item.seq[String](s => s(1))
  third <- item.seq[String](s => s(2))
} yield List(first, second, third).mkString
itemPickle2.unpickle.run("foo")
itemPickle2.pickle("foo", "")

val digitPickle = digit
digitPickle.unpickle.run("5")
digitPickle.unpickle.run("a")
digitPickle.pickle('5', "")
digitPickle.pickle('a', "")

val numberPickle = number
numberPickle.unpickle.run("345")
numberPickle.unpickle.run("45a6")
numberPickle.pickle("345", "")
numberPickle.pickle("45a6", "")

val patternPickle = for {
  s1 <- number.seq[(String, String, String)] { case (s, _, _) => s }
  s2 <- letter.atLeastOnce.seq[(String, String, String)] { case (_, s, _) => s.toList.filter(_.isLetter) }
  s3 <- number.seq[(String, String, String)] { case (_, _, s) => s }
} yield (s1, s2.mkString("[", "-", "]"), s3)
patternPickle.unpickle.run("45abc6")
patternPickle.pickle(("45", "[a-b-c]", "6"), "")

val stringPickle = string("hello")
stringPickle.unpickle.run("hello world")
stringPickle.pickle("hello", "world")

item.takeWhile(c => c == 'a').unpickle.run("aaabbc")
item.takeWhile(c => c == 'a').pickle(List('a', 'a', 'a'), "bbc")

number.separatedBy('-')(char('-')).unpickle.run("12-345-6789-")
number.separatedBy('-')(char('-')).pickle(List("12", "345", "6789"), "-")
number.separatedBy('-')(char('-')).pickle(Nil, "abc")

number.separatedBy1('-')(char('-')).unpickle.run("12-345-6789-")
number.separatedBy1('-')(char('-')).pickle(List("12", "345", "6789"), "-")
number.separatedBy1('-')(char('-')).unpickle.run("abc")
number.separatedBy1('-')(char('-')).pickle(Nil, "-")
