// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

package viper.silver.plugin.standard.doc

import viper.silver.plugin.SilverPlugin
import viper.silver.ast.Program
import viper.silver.ast.FilePosition
import viper.silver.ast.NoPosition
import viper.silver.ast.Position
import viper.silver.parser.{PProgram, PFieldDecl, PFields, PNode, PAnnotatedExp, PAnnotatedStmt, PAnnotation, PAtAnnotation, PDocAnnotation, PMethod, PFunction, PStringLiteral, PAxiom, PSpecification}
import viper.silver.parser.Translator
import viper.silver.ast.utility.Visitor
import viper.silver.parser.PAnnotated
import viper.silver.parser.PPredicate
import viper.silver.parser.PDomain
import viper.silver.parser.PDomainFunction
import upickle.default._
import java.io._

// from viperserver
import viper.silver.ast.{AbstractSourcePosition, Declaration, Domain, Field, Function, LocalVarDecl, Method, NamedDomainAxiom, Positioned, Predicate, Program, Scope, Typed }
import viper.silver.frontend.SilFrontend
import viper.silver.reporter._
import scala.language.postfixOps

class DocPlugin extends SilverPlugin {

  /** Called after parse AST has been constructed but before identifiers are
    * resolved and the program is type checked.
    *
    * @param input
    *   Parse AST
    * @return
    *   Modified Parse AST
    */
  override def beforeResolve(input: PProgram): PProgram = {
    println(input)
    println()
    println()
    println()
    input
  }


  case class DocNode(info: Map[String, String], pos: (String, String), doc: String, children: Seq[DocNode]) {
    /** @see [[Visitor.reduceTree()]] */
    def reduceTree[T](f: (DocNode, Seq[T]) => T) = Visitor.reduceTree(this, DocNode.callSubnodes)(f)

    /** @see [[Visitor.reduceWithContext()]] */
    def reduceWithContext[C, R](context: C, enter: (DocNode, C) => C, combine: (DocNode, C, Seq[R]) => R) = {
      Visitor.reduceWithContext(this, DocNode.callSubnodes)(context, enter, combine)
    }
  }

  object DocNode {
    def callSubnodes(n: DocNode): Seq[DocNode] = n.children
    implicit lazy val rw: ReadWriter[DocNode] = macroRW
  }

  /** Called after identifiers have been resolved but before the parse AST is
    * translated into the normal AST.
    *
    * @param input
    *   Parse AST
    * @return
    *   Modified Parse AST
      */
  override def beforeTranslate(input: PProgram): PProgram = {
    println(input)
    println(input.pretty)
    val translator: Translator = new Translator(input);

    val collectDocs: Seq[PAnnotation] => String = _.collect{ case k: PAtAnnotation if (k.key.str == "doc") => k.values.inner.toSeq.map(_.str)
      case k: PDocAnnotation => Seq(k.docString.str) }.flatten.mkString("\n")

    // TODO add back other relevant information
    // parameters, types, return type, etc.
    val extractInfo: PNode => Map[String, String] = {
      case n: PFields => Map("name" -> n.toString())
      case n: PFieldDecl => Map("name" -> n.idndef.name)
      case n: PMethod => Map("name" -> n.idndef.name)
      case n: PFunction => Map("name" -> n.idndef.name)
      case n: PPredicate => Map("name" -> n.idndef.name)
      case n: PDomain => Map("name" -> n.idndef.name)
      case n: PDomainFunction => Map("name" -> n.idndef.name)
      case n: PAxiom => Map("name" -> (n.idndef match {case Some(id) => id.name case None => ""}))
      case n: PSpecification[_] => Map("name" -> n.k.rs.display.trim())
      case _ => Map()
    }

    val extractPos: PNode => (String, String) = n => (n.pos._1.toString(), n.pos._2.toString())

    // TODO handle local var declarations
    val extractDoc: PNode => String =
      ({
         case e: PAnnotatedExp => translator.extractAnnotation(e)._2.getOrElse("doc", Seq()).mkString("\n")
         case s: PAnnotatedStmt => translator.extractAnnotationFromStmt(s)._2.getOrElse("doc", Seq()).mkString("\n")
         case n: PAnnotated => collectDocs(n.annotations)
         case n: PSpecification[_] => collectDocs(n.annotations)
         case n: PAxiom => collectDocs(n.annotations)
       }: PNode => String)//.andThen(flexmarkParser.parse(_)).andThen(flexmarkRenderer.render(_))

    val removeRoots: Seq[DocNode] => Seq[DocNode] = s => s.flatMap{
      case t: DocNode if (t.info.get("name") == Some("*root*")) => t.children
      case n: DocNode => Seq(n)
    }

    val docTree = input.reduceTree(
      {
        // case (n: PFields, children) => DocNode(n.toString(),  extractDoc(n), removeRoots(children))
        case (n: PFieldDecl, children) => DocNode(extractInfo(n), extractPos(n), extractDoc(n), removeRoots(children))

        case (n: PAxiom, children) => DocNode(extractInfo(n), extractPos(n),  extractDoc(n), removeRoots(children))
        case (n: PSpecification[_], children) => DocNode(extractInfo(n), extractPos(n),  extractDoc(n), removeRoots(children))

        case (n: PMethod, children) => DocNode(extractInfo(n), extractPos(n),  extractDoc(n), removeRoots(children))
        case (n: PFunction, children) => DocNode(extractInfo(n), extractPos(n),  extractDoc(n), removeRoots(children))
        case (n: PPredicate, children) => DocNode(extractInfo(n), extractPos(n),  extractDoc(n), removeRoots(children))
        case (n: PDomain, children) => DocNode(extractInfo(n), extractPos(n),  extractDoc(n), removeRoots(children))
        case (n: PDomainFunction, children) => DocNode(extractInfo(n), extractPos(n),  extractDoc(n), removeRoots(children))
        case (_, children) => DocNode(Map("name" -> "*root*"), ("", ""), "", removeRoots(children))
      }: (PNode, Seq[DocNode]) => DocNode)


    // val json: String = write(docTree)
    // println(json)
    // val jsonFile = "viperdoc-data.json"
    // val jsonWriter = new PrintWriter(new File(jsonFile))
    // try {
    //   jsonWriter.write(json)
    // } finally {
    //   jsonWriter.close()
    // }

    input
  }

  /** Called after parse AST has been translated into the normal AST but before
    * methods to verify are filtered. In [[viper.silver.frontend.SilFrontend]]
    * this step is confusingly called doTranslate.
    *
    * @param input
    *   AST
    * @return
    *   Modified AST
    */
  override def beforeMethodFilter(input: Program): Program = {
    println(input)
    // println(collect(input).mkString("\n"))
    input
  }
}
