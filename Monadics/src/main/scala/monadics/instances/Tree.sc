import monadics.instances.treeMonad.treeIsMonad
import monadics.instances.stateMonad.stateIsMonad
import monadics.instances.{Branch, Leaf}

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
