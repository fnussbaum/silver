// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

package viper.silver.plugin.standard.doc

import viper.silver.plugin.SilverPlugin
import viper.silver.ast.Program
import viper.silver.parser.PProgram

class DocPlugin extends SilverPlugin {
    /** Called before any processing happened.
      *
      * @param input Source code as read from file
      * @param isImported Whether the current input is an imported file or the main file
      * @return Modified source code
      */
    override def beforeParse(input: String, isImported: Boolean) : String = {

      // matches lines starting with /// including leading whitespace
      // TODO warning about docstrings with 
      val docPattern = """(^\s*///)(.*)""".r
      val transformedInput = input.linesIterator.map {
        case docPattern(_, docstring) => s"""@doc("${docstring}")"""
        case other => other
      }.mkString("\n")
      println(transformedInput)
      println()
      transformedInput
    }

    /** Called after parse AST has been constructed but before identifiers are resolved and the program is type checked.
      *
      * @param input Parse AST
      * @return Modified Parse AST
      */
    override def beforeResolve(input: PProgram) : PProgram = input

    /** Called after identifiers have been resolved but before the parse AST is translated into the normal AST.
      *
      * @param input Parse AST
      * @return Modified Parse AST
      */
    override def beforeTranslate(input: PProgram): PProgram = {
      println(input)
      input
    }

    /** Called after parse AST has been translated into the normal AST but before methods to verify are filtered.
      * In [[viper.silver.frontend.SilFrontend]] this step is confusingly called doTranslate.
      *
      * @param input AST
      * @return Modified AST
      */
    override def beforeMethodFilter(input: Program) : Program = {
      println(input)
      input
    }
}
