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
package com.github.rvanheest.starbux.client.cmd

import com.github.rvanheest.starbux.client.ConfigurationComponent
import org.rogach.scallop.{ ScallopConf, ScallopOption, Subcommand }

trait CommandLineInterfaceComponent {
  this: ConfigurationComponent =>

  val cli: CommandLineInterface

  case class CommandLineInterface(params: Seq[String]) extends ScallopConf(params) {
    appendDefaultToDescription = true
    editBuilder(_.setHelpWidth(110))

    printedName = "starbux-client"
    private val _________ = " " * printedName.length
    private val SUBCOMMAND_SEPARATOR = "---\n"
    private val description = s"A command line application to order and manage your Starbux coffee"
    private val synopsis =
      s"""
         |$printedName \\
         |${ _________ } order [-a addition...] <drink>""".stripMargin

    version(s"$printedName v${ configuration.version }")
    banner(
      s"""
         |$description
         |
         |Usage:
         |
         |$synopsis
         |
         |Options:
         |""".stripMargin)

    val order = new Subcommand("order") {
      descr("make an order to the StarBux service")
      val drink: ScallopOption[String] = trailArg(name = "drink",
        descr = "the drink to order")
      val additions: ScallopOption[List[String]] = opt(name = "additions", short = 'a',
        descr = "additions in the drink", default = Some(List.empty))
      footer(SUBCOMMAND_SEPARATOR)
    }

    addSubcommand(order)

    footer("")
  }
}
