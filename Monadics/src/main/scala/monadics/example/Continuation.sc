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

// TODO this one is not working yet!
def fooCCC[R](x: Int): Continuation[R, String] = Continuation.callCC[R, String, String](k => {
  val y = x * x + 3
  if (y > 20) k("over 20")
  Continuation(s"${y - 4}")
})

fooCCC(5).run(println)

