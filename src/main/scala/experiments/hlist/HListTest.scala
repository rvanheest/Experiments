package experiments.hlist

object HListTest extends App {
  val nil = HNil
  println(nil)

  val two: String :+: Int :+: HNil = HCons("a", HCons(1, HNil))
  println(two)

  println("abc" :+: "b" :+: 2 :+: HNil)

  val three : Int :+: String :+: Boolean :+: HNil = 10 :+: "hello" :+: true :+: HNil
  println(three)
}
