import monadics.instances.Continuation

def add(x: Int, y: Int): Int = x + y

def square(x: Int): Int = x * x

def pythagoras(x: Int, y: Int): Int = add(square(x), square(y))

pythagoras(3, 4)



def addCPS[R](x: Int, y: Int): Continuation[R, Int] = Continuation(add(x, y))

def squareCPS[R](x: Int): Continuation[R, Int] = Continuation(square(x))

def pythagorasCPS[R](x: Int, y: Int): Continuation[R, Int] = for {
  a <- squareCPS(x)
  b <- squareCPS(y)
  sum <- addCPS(a, b)
} yield sum

pythagorasCPS(3, 4).run(println(_))



def squareCCC[R](x: Int): Continuation[R, Int] = Continuation.callCC[R, Int, Int](k => k(x * x))

def fooCCC[R](x: Int): Continuation[R, String] = Continuation.callCC[R, String, String](k => {
  val y = x * x + 3
  if (y > 20) k("over 20")
  else Continuation(s"${y - 4}")
})

fooCCC(4).run(identity)
fooCCC(5).run(identity)

def barCCC[R](c: Char, s: String): Continuation[R, Int] = {
  Continuation.callCC[R, String, String](k => {
    c + s match {
      case "hello" => k("They say hello")
      case x => Continuation(s"They appear to be saying $x")
    }
  }).map(_.length)
}

barCCC('h', "ello").run(identity)
barCCC('f', "oobar").run(identity)

def quuxCCC[R]: Continuation[R, Int] = {
  Continuation.callCC[R, Int, Int](k => k(5).map(_ => 25))
}

quuxCCC.run(identity)
