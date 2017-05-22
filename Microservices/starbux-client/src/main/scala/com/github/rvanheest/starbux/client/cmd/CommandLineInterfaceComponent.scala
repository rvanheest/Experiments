/**
 * Copyright (C) 2017 Richard van Heest (richard.v.heest@gmail.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.rvanheest.starbux.client.cmd

import com.github.rvanheest.starbux.client.PropertiesComponent
import org.rogach.scallop.{ ScallopConf, Subcommand }

trait CommandLineInterfaceComponent {
  this: PropertiesComponent =>

  val cli: CommandLineInterface

  case class CommandLineInterface(params: Seq[String]) extends ScallopConf(params) {
    appendDefaultToDescription = true
    editBuilder(_.setHelpWidth(110))
    printedName = "starbux-client"
    private val _________ = " " * printedName.length
    private val SUBCOMMAND_SEPARATOR = "---\n"
    private val description = s"""A polite module that greets you"""
    private val synopsis =
      s"""$printedName \\
         |      <synopsis of command line parameters> \\
         |      <...possibly continued again, or all joined on one line>""".stripMargin

    version(s"$printedName v${properties.version}")
    banner(
      s"""
         |  $description
         |
         |Usage:
         |
         |  $synopsis
         |
         |Options:
         |""".stripMargin)

    val sc1 = new Subcommand("sc1") {
      descr("description")
      footer(SUBCOMMAND_SEPARATOR)
    }
    addSubcommand(sc1)

    footer("")
  }
}
