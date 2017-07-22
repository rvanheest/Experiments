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

import better.files.File
import org.apache.commons.configuration.PropertiesConfiguration

trait PropertiesComponent {

  val properties: GeneralProperties

  trait GeneralProperties {
    def version: String
    def properties: PropertiesConfiguration
  }

  object GeneralProperties {
    def apply(home: File): GeneralProperties = new GeneralProperties {
      override val version: String = (home / "version").contentAsString
      override val properties = new PropertiesConfiguration((home / "cfg" / "application.properties").toJava)
    }
  }
}
