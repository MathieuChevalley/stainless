/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction

package object innerfuns {

  object trees extends Trees with inox.ast.SimpleSymbols {
    case class Symbols(
      functions: Map[Identifier, FunDef],
      sorts: Map[Identifier, ADTSort]
    ) extends SimpleSymbols with AbstractSymbols

    object printer extends Printer { val trees: innerfuns.trees.type = innerfuns.trees }
  }

  def extractor(implicit ctx: inox.Context) = FunctionClosure(trees, inlining.trees)
}
