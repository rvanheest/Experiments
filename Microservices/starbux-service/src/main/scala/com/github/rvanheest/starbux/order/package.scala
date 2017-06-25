package com.github.rvanheest.starbux

import java.util.UUID

package object order {

  type ID = UUID

  case class Order(status: Status, drinks: List[Drink])
  case class Drink(id: ID = UUID.randomUUID(), drink: String, addition: List[Addition])
  type Addition = String

  sealed abstract class Status(name: String)
  case object Ordered extends Status("Ordered")
  case object Prepared extends Status("Prepared")
  case object Payed extends Status("Payed")
  case object Served extends Status("Served")
}
