package monadics.test.instances

import monadics.instances.Continuation

class ContinuationSpec extends InstanceSpec {

  def addCPS[R](x: Double, y: Double): Continuation[R, Double] = {
    Continuation.from(x + y)
  }

  def squareCPS[R](x: Double): Continuation[R, Double] = {
    Continuation.from(x * x)
  }

  def pythagorasCPS[R](x: Double, y: Double): Continuation[R, Double] = {
    for {
      a <- squareCPS(x)
      b <- squareCPS(y)
      sum <- addCPS(a, b)
    } yield sum
  }

  property("pythagoras CPS") {
    forAll { (x: Double, y: Double) =>
      pythagorasCPS(x, y).run(identity) shouldBe (x * x) + (y * y)
    }
  }

  def fooCCC[R](x: Int): Continuation[R, String] = Continuation.callCC[R, String, String](k => {
    val y = x * x + 3
    if (y > 20) k("over 20")
    else Continuation.from((y - 4).toString)
  })

  property("fooCCC CPS") {
    forAll { (x: Int) =>
      val y = x * x + 3
      fooCCC(x).run(identity) shouldBe (if (y > 20) "over 20" else (y - 4).toString)
    }
  }
}
