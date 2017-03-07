import experiments.pickling.StringPickler

val pickler = new StringPickler {}
import pickler._

base

belowBase.pickle(97)
belowBase.unpickle("a")

belowBase.pair(belowBase).pickle((97, 98))
belowBase.pair(belowBase).unpickle("ab")

zeroTo(100).pickle(97)
zeroTo(100).unpickle("a")

char.pickle('z')
char.unpickle("z")
