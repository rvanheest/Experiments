import monadics.instances.State.stateIsMonad
import monadics.instances.Tree._
import monadics.instances.{Branch, Leaf}
import monadics.instances.monoids.values._

val tree = Branch(
	Leaf("a"),
	Branch(
		Leaf("b"),
		Leaf("c")
	)
)
val mapped = tree.map(_.toUpperCase)
val zipped = tree.zipTree(mapped).get
val number = tree.number.evaluate(0)

treeIsEquals[String](stringIsEquals).equals(tree, tree)
treeIsEquals[String].equals(tree, mapped)
treeIsEquals[String].equals(tree, tree.flatMap(s => mapped.map(S => s"$s$S")))
