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

import java.nio.file.{ Files, Paths }

import better.files.File
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration

import scala.util.Properties

trait ConfigurationComponent {
  this: DebugEnhancedLogging =>

  val configuration: Configuration

  trait Configuration {
    def version: String

    def properties: PropertiesConfiguration
  }

  object Configuration {
    def apply(): Configuration = new Configuration {
      private val home = File(Properties.propOrEmpty("app.home"))

      override lazy val version: String = (home / "version").contentAsString

      private val cfgPath = Seq(Paths.get(s"/etc/opt/starbux.nl/starbux-client/"), (home / "cfg").path)
        .find(Files.exists(_))
        .getOrElse { throw new IllegalStateException("No configuration directory found") }
      override val properties = new PropertiesConfiguration(cfgPath.resolve("application.properties").toFile)
    }
  }
}
