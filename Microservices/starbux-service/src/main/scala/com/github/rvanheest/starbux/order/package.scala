/**
 * Copyright (C) 2017 Richard van Heest
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.rvanheest.starbux

import java.util.UUID

package object order {

  type ID = UUID
  type OrderId = Int
  type Cost = Int

  case class Order(status: Status, drinks: Set[Drink])
  case class Drink(id: ID = UUID.randomUUID(), drink: String, additions: List[Addition] = List.empty) {
    override def equals(other: Any): Boolean = {
      other match {
        case Drink(thatId, thatDrink, thoseAdditions) =>
          id == thatId &&
            drink == thatDrink &&
            additions.sorted == thoseAdditions.sorted
        case _ => false
      }
    }
  }
  type Addition = String

  sealed abstract class Status(name: String)
  case object Ordered extends Status("Ordered")
  case object Prepared extends Status("Prepared")
  case object Payed extends Status("Payed")
  case object Served extends Status("Served")
  object Status {
    def fromString(s: String): Option[Status] = s match {
      case "Ordered" => Some(Ordered)
      case "Prepared" => Some(Prepared)
      case "Payed" => Some(Payed)
      case "Served" => Some(Served)
      case _ => None
    }
  }

  case class UnknownOrderException(orderId: OrderId) extends Exception(s"Order $orderId does not exist.")
  case class UnknownItemException(msg: String) extends Exception(msg)
  case class UnknownOrderStateException(orderId: OrderId) extends Exception(s"Order status is unknown for $orderId.")
  case class EmptyRequestException() extends Exception("The order did not contain any drinks.")
}
