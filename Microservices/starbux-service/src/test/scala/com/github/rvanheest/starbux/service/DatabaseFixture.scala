package com.github.rvanheest.starbux.service

import java.nio.file.{ Files, Path }
import java.sql.Connection
import java.util.UUID

import com.github.rvanheest.starbux.DatabaseAccessComponent
import com.github.rvanheest.starbux.order.ID
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatest.BeforeAndAfterEach
import resource._

import scala.io.Source
import scala.util.Success

trait DatabaseFixture extends TestSupportFixture
  with BeforeAndAfterEach
  with DatabaseAccessComponent
  with DebugEnhancedLogging {

  implicit var connection: Connection = _

  val databaseFile: Path = testDir.resolve("database.db")

  override val databaseAccess = new DatabaseAccess {
    override val dbDriverClassName: String = "org.sqlite.JDBC"
    override val dbUrl: String = s"jdbc:sqlite:${ databaseFile.toString }"
    override val dbUsername = Option.empty[String]
    override val dbPassword = Option.empty[String]

    override protected def createConnectionPool: ConnectionPool = {
      val pool = super.createConnectionPool

      managed(pool.getConnection)
        .flatMap(connection => managed(connection.createStatement))
        .and(managed(Source.fromFile(getClass.getClassLoader.getResource("database/database.sql").toURI)).map(_.mkString))
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
    Files.deleteIfExists(databaseFile)
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
