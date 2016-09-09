import monadics.instances.State

type Stack = List[Int]

def pop: State[Stack, Int] = new State({ case x :: xs => (x, xs) })

def push(a: Int): State[Stack, Unit] = new State(stack => ((), a :: stack))

val forComprehension = for {
	_ <- push(100)
	a <- pop
	_ <- push(100)
	b <- pop
} yield (a, b)

val monadicChain = push(100) andThen pop flatMap (a => push(100) andThen pop map ((a, _)))

val f = new State[Stack, Int => Int => (Int, Int)](stack => (a => b => (a, b), stack))
val applicativeChain = f <* push(100) <*> pop <* push(100) <*> pop


forComprehension.run(List(1, 2, 3))
monadicChain.run(List(1, 2, 3))
applicativeChain.run(List(1, 2, 3))
