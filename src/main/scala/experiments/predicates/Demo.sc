import experiments.predicates.Predicate._

val p1 = (i: Int) => i > 0
val p2 = (i: Int) => i % 2 == 0

val p3 = !p1
val p4 = p1 and p2
val p5 = p1 or p2
val p6 = p1 xor p2
val p7 = p1 nor p2
val p8 = p1 nand p2
val p9 = p1 xnor p2
val p10 = p1 -> p2
val p11 = p1 <-> p2

test(p3)(0, 1)
test(p4)(-2, 1, 2)

List(-2, 1, 2, 3, 4).filter(p4)
