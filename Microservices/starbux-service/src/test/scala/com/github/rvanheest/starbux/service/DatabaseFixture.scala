package com.github.rvanheest.starbux.service

import java.nio.file.{ Files, Path }
import java.sql.Connection

import com.github.rvanheest.starbux.DatabaseAccessComponent
import com.github.rvanheest.starbux.order.DatabaseComponent
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.scalatest.BeforeAndAfterEach

import scala.io.Source
import resource._

import scala.util.Success

trait DatabaseFixture extends TestSupportFixture
  with BeforeAndAfterEach
  with DatabaseAccessComponent
  with DatabaseComponent
  with DebugEnhancedLogging {

  implicit var connection: Connection = _

  val databaseFile: Path = testDir.resolve("database.db")

  override val databaseAccess = new DatabaseAccess {
    override val dbDriverClassName: String = "org.sqlite.JDBC"
    override val dbUrl: String = s"jdbc:sqlite:${databaseFile.toString}"
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
}
