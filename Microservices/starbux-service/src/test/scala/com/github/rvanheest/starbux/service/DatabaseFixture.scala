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
package com.github.rvanheest.starbux.service

import java.sql.Connection
import java.util.UUID

import better.files.File
import com.github.rvanheest.starbux.DatabaseAccessComponent
import com.github.rvanheest.starbux.order.{DatabaseComponent, ID}
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatest.BeforeAndAfterEach
import resource._

import scala.util.Success

trait DatabaseFixture extends BeforeAndAfterEach with DatabaseAccessComponent with DebugEnhancedLogging {
  this: TestSupportFixture =>

  implicit var connection: Connection = _

  val databaseFile: File = testDir / "database.db"

  override val databaseAccess = new DatabaseAccess {
    override val dbDriverClassName: String = "org.sqlite.JDBC"
    override val dbUrl: String = s"jdbc:sqlite:${ databaseFile.toString }"
    override val dbUsername = Option.empty[String]
    override val dbPassword = Option.empty[String]

    override protected def createConnectionPool: ConnectionPool = {
      val pool = super.createConnectionPool

      managed(pool.getConnection)
        .flatMap(connection => managed(connection.createStatement))
        .and(constant(File(getClass.getClassLoader.getResource("database/database.sql").toURI).contentAsString))
        .map { case (statement, query) =>
          statement.executeUpdate(query)
        }
        .tried shouldBe a[Success[_]]

      connection = pool.getConnection

      pool
    }
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    if (databaseFile.exists) databaseFile.delete()
    databaseAccess.initConnectionPool()
  }

  override def afterEach(): Unit = {
    connection.close()
    databaseAccess.closeConnectionPool()
    super.afterEach()
  }

  def inspectOrderTable: List[(Int, Int)] = {
    val resultSet = for {
      prepStatement <- managed(connection.prepareStatement("SELECT * FROM `Order`;"))
      resultSet <- managed(prepStatement.executeQuery())
    } yield resultSet

    resultSet
      .acquireAndGet(result => Stream.continually(result.next())
        .takeWhile(b => b)
        .map(_ => {
          (result.getInt("orderId"), result.getInt("statusId"))
        })
        .toList)
  }

  def inspectOrderDrinkTable: List[(ID, Int, Int, Int)] = {
    val resultSet = for {
      prepStatement <- managed(connection.prepareStatement("SELECT * FROM order_drink;"))
      resultSet <- managed(prepStatement.executeQuery())
    } yield resultSet

    resultSet
      .acquireAndGet(result => Stream.continually(result.next())
        .takeWhile(b => b)
        .map(_ => {
          (UUID.fromString(result.getString("id")), result.getInt("orderId"), result.getInt("drinkId"), result.getInt("drinkCost"))
        })
        .toList)
  }

  def inspectOrderDrinkAdditionsTable: List[(ID, Int, Int)] = {
    val resultSet = for {
      prepStatement <- managed(connection.prepareStatement("SELECT * FROM order_drink_additions;"))
      resultSet <- managed(prepStatement.executeQuery())
    } yield resultSet

    resultSet
      .acquireAndGet(result => Stream.continually(result.next())
        .takeWhile(b => b)
        .map(_ => {
          (UUID.fromString(result.getString("orderDrinkId")), result.getInt("additionId"), result.getInt("additionCost"))
        })
        .toList)
  }
}
