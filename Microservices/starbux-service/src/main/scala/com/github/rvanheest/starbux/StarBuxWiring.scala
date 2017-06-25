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

import java.nio.file.Paths

import com.github.rvanheest.starbux.order.DatabaseComponent
import com.github.rvanheest.starbux.service.ServerWiring
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

trait StarBuxWiring extends ServerWiring
  with DatabaseComponent
  with DatabaseAccessComponent
  with PropertiesComponent
  with DebugEnhancedLogging {

  private lazy val home = Paths.get(System.getProperty("app.home"))

  override lazy val properties: GeneralProperties = GeneralProperties(home)
  override lazy val databaseAccess: DatabaseAccess = new DatabaseAccess {
    override val dbDriverClassName: String = properties.properties.getString("starbux-service.database.driver-class")
    override val dbUrl: String = properties.properties.getString("starbux-service.database.url")
    override val dbUsername: Option[String] = Option(properties.properties.getString("starbux-service.database.username"))
    override val dbPassword: Option[String] = Option(properties.properties.getString("starbux-service.database.password"))
  }
  override lazy val database: Database = new Database {}
}
