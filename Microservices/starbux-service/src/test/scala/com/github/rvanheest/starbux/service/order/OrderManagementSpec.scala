package com.github.rvanheest.starbux.service.order

import java.sql.Connection

import com.github.rvanheest.starbux.order._
import com.github.rvanheest.starbux.service.TestSupportFixture
import nl.knaw.dans.lib.error.CompositeException
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalamock.scalatest.MockFactory

import scala.util.{Failure, Success, Try}

class OrderManagementSpec extends TestSupportFixture
  with MockFactory
  with OrderManagementComponent
  with DatabaseComponent
  with DebugEnhancedLogging {

  implicit private val connection: Connection = mock[Connection]
  override val database: Database = mock[Database]
  override val orderManagement: OrderManagement = new OrderManagement {}

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
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (*, *, *) never()
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(order1) should matchPattern { case Success(`orderId`) => }
  }

  it should "insert an order with one drink and no additions on it" in {
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Prepared, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink1, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(order2) should matchPattern { case Success(`orderId`) => }
  }

  it should "insert an order with one drink and one addition on it" in {
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Payed, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink2, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (add1, drink2.id, *) once() returning Try { () }

    orderManagement.addOrder(order3) should matchPattern { case Success(`orderId`) => }
  }

  it should "insert an order with multiple drinks and additions on them" in {
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Served, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink3, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (add1, drink3.id, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (add2, drink3.id, *) once() returning Try { () }
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink4, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (add3, drink4.id, *) once() returning Try { () }

    orderManagement.addOrder(order4) should matchPattern { case Success(`orderId`) => }
  }

  it should "fail if the drink is unknown" in {
    val drink = Drink(drink = "cola")
    val invalidOrder = Order(Ordered, List(drink))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: cola"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(UnknownItemException("Unknown drink: cola")) =>
    }
  }

  it should "fail if multiple drinks are unknown" in {
    val drink1 = Drink(drink = "cola")
    val drink2 = Drink(drink = "ice tea")
    val invalidOrder = Order(Ordered, List(drink1, drink2))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink1, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: cola"))
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink2, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: ice tea"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(CompositeException(UnknownItemException("Unknown drink: cola") :: UnknownItemException("Unknown drink: ice tea") :: Nil)) =>
    }
  }

  it should "fail if the addition is unknown" in {
    val drink = Drink(drink = "coffee", additions = List("whiskey"))
    val invalidOrder = Order(Ordered, List(drink))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("whiskey", drink.id, *) once() returning Failure(UnknownItemException("Unknown addition: whiskey"))

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(UnknownItemException("Unknown addition: whiskey")) =>
    }
  }

  it should "fail if multiple additions are unknown" in {
    val drink = Drink(drink = "coffee", additions = List("whiskey", "vodka"))
    val invalidOrder = Order(Ordered, List(drink))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("whiskey", drink.id, *) once() returning Failure(UnknownItemException("Unknown addition: whiskey"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("vodka", drink.id, *) once() returning Failure(UnknownItemException("Unknown addition: vodka"))

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(CompositeException(UnknownItemException("Unknown addition: whiskey") :: UnknownItemException("Unknown addition: vodka") :: Nil)) =>
    }
  }

  it should "fail if both drink and addition are unknown" in {
    val drink = Drink(drink = "cola", additions = List("whiskey"))
    val invalidOrder = Order(Ordered, List(drink))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: cola"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("whiskey", drink.id, *) never()

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(UnknownItemException("Unknown drink: cola")) =>
    }
  }

  it should "fail if some drinks and addition are unknown" in {
    val drink1 = Drink(drink = "coffee", additions = List("whiskey"))
    val drink2 = Drink(drink = "cola")
    val invalidOrder = Order(Ordered, List(drink1, drink2))
    val orderId = 1
    (database.addToOrderTable(_: Status)(_: Connection)) expects (Ordered, *) once() returning Try(orderId)
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink1, orderId, *) once() returning Try { () }
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects ("whiskey", drink1.id, *) once() returning Failure(UnknownItemException("Unknown addition: whiskey"))
    (database.addToOrderDrinkTable(_: Drink, _: OrderId)(_: Connection)) expects (drink2, orderId, *) once() returning Failure(UnknownItemException("Unknown drink: cola"))
    (database.addToOrderDrinkAdditionTable(_: Addition, _: ID)(_: Connection)) expects (*, *, *) never()

    orderManagement.addOrder(invalidOrder) should matchPattern {
      case Failure(CompositeException(UnknownItemException("Unknown addition: whiskey") :: UnknownItemException("Unknown drink: cola") :: Nil)) =>
    }
  }

  "calculateCost" should "calculate the cost of an empty order" in {
    val orderId = 1
    (database.costView(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      (drink1.id, 0, 0) :: Nil
    }

    orderManagement.calculateCost(orderId) should matchPattern { case Success(0) => }
  }

  it should "calculate the cost of an order with one drink and no additions on it" in {
    val orderId = 1
    (database.costView(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      (drink1.id, 1, 0) :: Nil
    }

    orderManagement.calculateCost(orderId) should matchPattern { case Success(1) => }
  }

  it should "calculate the cost of an order with one drink and one addition on it" in {
    val orderId = 1
    (database.costView(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      (drink2.id, 1, 1) :: Nil
    }

    orderManagement.calculateCost(orderId) should matchPattern { case Success(2) => }
  }

  it should "calculate the cost of an order with multiple drinks and additions on them" in {
    val orderId = 1
    (database.costView(_: OrderId)(_: Connection)) expects (orderId, *) once() returning Try {
      (drink3.id, 1, 1) :: (drink3.id, 1, 1) :: (drink4.id, 2, 2) :: Nil
    }

    orderManagement.calculateCost(orderId) should matchPattern { case Success(7) => }
  }
}
