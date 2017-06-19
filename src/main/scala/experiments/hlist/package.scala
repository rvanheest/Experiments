package experiments

package object hlist {
  val HNil = new HNil()

  type :+:[H, T <: HList] = HCons[H, T]
  val :+: = HCons
}
