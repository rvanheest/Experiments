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
package com.github.rvanheest.starbux.service.order

import com.github.rvanheest.starbux.order._
import com.github.rvanheest.starbux.service.{DatabaseFixture, TestSupportFixture}
import nl.knaw.dans.lib.error.CompositeException

import scala.util.{Failure, Success}

class OrderWiringSpec extends TestSupportFixture with DatabaseFixture with OrderWiring {
  import databaseAccess.doTransaction

  private val add1 = "sugar"
  private val add2 = "milk"
  private val add3 = "honey"

  private val drink1 = Drink(drink = "coffee")
  private val drink2 = Drink(drink = "coffee", additions = List(add1))
  private val drink3 = Drink(drink = "coffee", additions = List(add1, add2))
  private val drink4 = Drink(drink = "tea", additions = List(add3))

  private val order1 = Order(Ordered, List.empty)
  private val order2 = Order(Prepared, List(drink1))
  private val order3 = Order(Payed, List(drink2))
  private val order4 = Order(Served, List(drink3, drink4))

  "addOrder" should "insert an empty order" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => orderManagement.addOrder(order1)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    inspectOrderTable should (have size 1 and contain only((expectedOrderId, 1)))
  }

  it should "insert an order with one drink and no additions on it" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => orderManagement.addOrder(order2)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    inspectOrderTable should (have size 1 and contain only((expectedOrderId, 2)))
    inspectOrderDrinkTable should (have size 1 and contain only((drink1.id, 1, 1, 1)))
  }

  it should "insert an order with one drink and one addition on it" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => orderManagement.addOrder(order3)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    inspectOrderTable should (have size 1 and contain only((expectedOrderId, 3)))
    inspectOrderDrinkTable should (have size 1 and contain only((drink2.id, 1, 1, 1)))
    inspectOrderDrinkAdditionsTable should (have size 1 and contain only((drink2.id, 2, 1)))
  }

  it should "insert an order with multiple drinks and additions on them" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => orderManagement.addOrder(order4)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    inspectOrderTable should (have size 1 and contain only((expectedOrderId, 4)))
    inspectOrderDrinkTable should (have size 2 and contain only((drink3.id, 1, 1, 1), (drink4.id, 1, 2, 2)))
    inspectOrderDrinkAdditionsTable should (have size 3 and contain only((drink3.id, 2, 1), (drink3.id, 1, 1), (drink4.id, 3, 2)))
  }

  it should "fail if the drink is unknown" in {
    val invalidOrder = Order(Ordered, List(Drink(drink = "cola")))
    doTransaction(implicit connection => orderManagement.addOrder(invalidOrder)) should matchPattern {
      case Failure(UnknownItemException("Unknown drink: cola")) =>
    }

    inspectOrderTable shouldBe empty
    inspectOrderDrinkTable shouldBe empty
    inspectOrderDrinkAdditionsTable shouldBe empty
  }

  it should "fail if multiple drinks are unknown" in {
    val invalidOrder = Order(Ordered, List(Drink(drink = "cola"), Drink(drink = "ice tea")))
    doTransaction(implicit connection => orderManagement.addOrder(invalidOrder)) should matchPattern {
      case Failure(CompositeException(UnknownItemException("Unknown drink: cola") :: UnknownItemException("Unknown drink: ice tea") :: Nil)) =>
    }

    inspectOrderTable shouldBe empty
    inspectOrderDrinkTable shouldBe empty
    inspectOrderDrinkAdditionsTable shouldBe empty
  }

  it should "fail if the addition is unknown" in {
    val invalidOrder = Order(Ordered, List(Drink(drink = "coffee", additions = List("whiskey"))))
    doTransaction(implicit connection => orderManagement.addOrder(invalidOrder)) should matchPattern {
      case Failure(UnknownItemException("Unknown addition: whiskey")) =>
    }

    inspectOrderTable shouldBe empty
    inspectOrderDrinkTable shouldBe empty
    inspectOrderDrinkAdditionsTable shouldBe empty
  }

  it should "fail if multiple additions are unknown" in {
    val invalidOrder = Order(Ordered, List(Drink(drink = "coffee", additions = List("whiskey", "vodka"))))
    doTransaction(implicit connection => orderManagement.addOrder(invalidOrder)) should matchPattern {
      case Failure(CompositeException(UnknownItemException("Unknown addition: whiskey") :: UnknownItemException("Unknown addition: vodka") :: Nil)) =>
    }

    inspectOrderTable shouldBe empty
    inspectOrderDrinkTable shouldBe empty
    inspectOrderDrinkAdditionsTable shouldBe empty
  }

  it should "fail if both drink and addition are unknown" in {
    val invalidOrder = Order(Ordered, List(Drink(drink = "cola", additions = List("whiskey"))))
    doTransaction(implicit connection => orderManagement.addOrder(invalidOrder)) should matchPattern {
      case Failure(UnknownItemException("Unknown drink: cola")) =>
    }

    inspectOrderTable shouldBe empty
    inspectOrderDrinkTable shouldBe empty
    inspectOrderDrinkAdditionsTable shouldBe empty
  }

  it should "fail if some drinks and addition are unknown" in {
    val invalidOrder = Order(Ordered, List(Drink(drink = "coffee", additions = List("whiskey")), Drink(drink = "cola")))
    doTransaction(implicit connection => orderManagement.addOrder(invalidOrder)) should matchPattern {
      case Failure(CompositeException(UnknownItemException("Unknown addition: whiskey") :: UnknownItemException("Unknown drink: cola") :: Nil)) =>
    }

    inspectOrderTable shouldBe empty
    inspectOrderDrinkTable shouldBe empty
    inspectOrderDrinkAdditionsTable shouldBe empty
  }

  "calculateCost" should "calculate the cost of an empty order" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => orderManagement.addOrder(order1)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    orderManagement.calculateCost(expectedOrderId) should matchPattern { case Success(0) => }
  }

  it should "calculate the cost of an order with one drink and no additions on it" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => orderManagement.addOrder(order2)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    orderManagement.calculateCost(expectedOrderId) should matchPattern { case Success(1) => }
  }

  it should "calculate the cost of an order with one drink and one addition on it" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => orderManagement.addOrder(order3)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    orderManagement.calculateCost(expectedOrderId) should matchPattern { case Success(2) => }
  }

  it should "calculate the cost of an order with multiple drinks and additions on them" in {
    val expectedOrderId = 1
    doTransaction(implicit connection => orderManagement.addOrder(order4)) should matchPattern {
      case Success(`expectedOrderId`) =>
    }

    orderManagement.calculateCost(expectedOrderId) should matchPattern { case Success(7) => }
  }
}
