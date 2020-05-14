import experiments.prettyprinter.Doc._
import experiments.prettyprinter.{Cons, Line, Nest, Text}

val doc1 =
  Cons(Text("def theAnswer {"), Cons(Line,
    Cons(Text("  var i = 42"), Cons(Line,
      Cons(Text("  println(i)"), Cons(Line,
        Text("}")))))))

doc1.layout

val doc2 =
  Cons(Text("def theAnswer {"), Cons(Nest(2, Cons(Line,
    Cons(Text("  var i = 42"), Cons(Line,
      Text("  println(i)"))))), Cons(Line,
    Text("}"))))

doc2.layout

val doc3 =
  "def theAnswer" ~ block(
    "var i = 42" <~
      "println(i)"
  )

doc3.layout
