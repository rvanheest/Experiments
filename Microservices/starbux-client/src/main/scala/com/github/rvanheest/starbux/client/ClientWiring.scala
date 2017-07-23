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
package com.github.rvanheest.starbux.client

import java.net.URL

import better.files.File
import com.github.rvanheest.starbux.client.cmd.{ CommandLineInterfaceComponent, CommandLineRunComponent }
import com.github.rvanheest.starbux.client.logic.{ HttpConnectionComponent, OrdererComponent }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

class ClientWiring(params: Seq[String]) extends CommandLineInterfaceComponent
                                                with CommandLineRunComponent
                                                with OrdererComponent
                                                with HttpConnectionComponent
                                                with ConfigurationComponent
                                                with DebugEnhancedLogging {

  private lazy val home = File.home

  override lazy val configuration: Configuration = Configuration(home)
  override lazy val cli: CommandLineInterface = CommandLineInterface(params)
  override lazy val httpConnection: HttpConnection = new HttpConnection {
    private val key = "client.service.baseUrl"
    private val value = configuration.properties.getString(key)
    if (!value.endsWith("/")) {
      logger.warn(s"adding a '/' to the end of property '$key'")
      configuration.properties.setProperty(key, value + "/")
      configuration.properties.save()
    }

    val baseUrl: URL = new URL(configuration.properties.getString("client.service.baseUrl"))
  }
  override lazy val orderer: Orderer = new Orderer {}
  override lazy val run: CommandLineRun = new CommandLineRun {}
}