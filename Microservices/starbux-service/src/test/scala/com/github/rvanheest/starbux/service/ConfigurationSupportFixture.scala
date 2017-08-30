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

import better.files.File
import com.github.rvanheest.starbux.ConfigurationComponent
import org.apache.commons.configuration.PropertiesConfiguration

trait ConfigurationSupportFixture extends ConfigurationComponent {
  this: TestSupportFixture =>

  override val configuration: Configuration = {
    val propsFile = (testDir / "cfg").createDirectories() / "application.properties"
    File(getClass.getResource("/debug-config/application.properties").toURI).copyTo(propsFile)

    new Configuration {
      def version: String = "version x.y.z"
      def properties: PropertiesConfiguration = new PropertiesConfiguration(propsFile.toJava)
    }
  }
}
