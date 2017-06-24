package com.github.rvanheest.starbux

import java.util.UUID

package object order {

  type ID = UUID

  case class Order(status: Status, drinks: List[Drink])
  case class Drink(id: ID = UUID.randomUUID(), drink: String, addition: List[Addition])
  type Addition = String

//  type ID = UUID
//  type OrderId = Int
//  case class Order(drinks: List[Drink], status: Status) {
//    lazy val cost: Int = {
//      drinks.foldRight(List.empty[Int])((drink, costs) => {
//        drink.cost :: drink.additions.foldRight(costs)(_.cost :: _)
//      }).sum
//    }
//  }
//  case class Drink(id: ID = UUID.randomUUID(), drink: String, additions: List[Addition], cost: Int)
//  case class Addition(id: ID = UUID.randomUUID(), add: String, cost: Int)

  sealed abstract class Status(name: String)
  case object Ordered extends Status("Ordered")
  case object Prepared extends Status("Prepared")
  case object Payed extends Status("Payed")
  case object Served extends Status("Served")
}
