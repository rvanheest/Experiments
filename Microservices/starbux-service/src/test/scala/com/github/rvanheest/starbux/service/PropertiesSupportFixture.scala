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

import java.nio.file.{ Files, Paths }

import com.github.rvanheest.starbux.PropertiesComponent

import scala.collection.JavaConverters._

trait PropertiesSupportFixture extends TestSupportFixture with PropertiesComponent {

  override val properties: GeneralProperties = {
    val versionFile = testDir.resolve("version")
    Files.createFile(versionFile)
    Files.write(versionFile, List("version x.y.z").asJava)

    Files.createDirectory(testDir.resolve("cfg/"))
    Files.copy(
      Paths.get(getClass.getResource("/debug-config/application.properties").toURI),
      testDir.resolve("cfg/application.properties"))

    GeneralProperties(testDir)
  }
}
