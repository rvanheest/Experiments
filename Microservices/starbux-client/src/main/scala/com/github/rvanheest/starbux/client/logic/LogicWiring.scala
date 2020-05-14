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
package com.github.rvanheest.starbux.client.logic

import java.net.URI

import com.github.rvanheest.starbux.client.ConfigurationComponent
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

trait LogicWiring extends HttpConnectionComponent with OrdererComponent {
  this: ConfigurationComponent with DebugEnhancedLogging =>

  override lazy val httpConnection: HttpConnection = new HttpConnection {
    private val key = "client.service.baseUrl"
    private val value = configuration.properties.getString(key)

    if (!value.endsWith("/")) {
      logger.warn(s"adding a '/' to the end of property '$key'")
      configuration.properties.setProperty(key, value + "/")
      configuration.properties.save()
    }

    val baseUri: URI = new URI(configuration.properties.getString(key))
  }
  override lazy val orderer: Orderer = new Orderer {}
}
