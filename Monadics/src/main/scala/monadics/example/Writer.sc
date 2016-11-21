import monadics.instances.Writer
import monadics.instances.Writer.tell
import monadics.instances.list._

type Log = List[String]
def log(s: String): Log = s :: Nil

def logNumber(x: Int): Writer[Log, Int] = {
  Writer(x, log(s"Got number: $x"))
}

val multWithLog: Writer[Log, Int] = for {
  a <- logNumber(5)
  b <- logNumber(3)
  _ <- tell(log(s"Gonna multiply $a and $b"))
} yield a * b

multWithLog.run
multWithLog.value
multWithLog.log.mkString("\n")

def gcd(a: Int, b: Int): Writer[Log, Int] = {
  if (b == 0)
    tell(log(s"Finished with $a")).map(_ => a)
  else
    for {
      _ <- tell(log(s"$a % $b = ${a % b}"))
      result <- gcd(b, a % b)
    } yield result
}

gcd(8, 1).run
gcd(8, 2).run
gcd(8, 3).run
gcd(8, 4).run
