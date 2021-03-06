/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package solvers

import inox.solvers._
import inox.solvers.combinators._

import evaluators._

object SolverFactory {

  def getFromName(name: String)
                 (p: Program, ctx: inox.Context)
                 (enc: inox.ast.ProgramTransformer {
                    val sourceProgram: p.type
                    val targetProgram: StainlessProgram
                  })(implicit sem: p.Semantics): SolverFactory { val program: p.type; type S <: TimeoutSolver { val program: p.type } } = {
    if (inox.solvers.SolverFactory.solvers(name)) {
      val newCtx: inox.Context =
        if (ctx.options.findOption(optAssumeChecked).isDefined) ctx
        else ctx.withOpts(optAssumeChecked(true))

      inox.solvers.SolverFactory.getFromName(name)(p, newCtx)(
        enc andThen InoxEncoder(enc.targetProgram, ctx)
      )
    } else {
      sys.error("TODO!")
    }
  }

  // Note that in case of a portfolio solver, we will share the evaluator and encoder
  // between all underlying solvers. We count on immutability to ensure sanity here.
  def getFromNames(names: Seq[String])
                  (p: Program, ctx: inox.Context)
                  (enc: inox.ast.ProgramTransformer {
                     val sourceProgram: p.type
                     val targetProgram: StainlessProgram
                   })(implicit sem: p.Semantics): SolverFactory { val program: p.type; type S <: TimeoutSolver { val program: p.type } } = {
    names match {
      case Seq() => throw new inox.FatalError("No selected solver")
      case Seq(single) => getFromName(single)(p, ctx)(enc)
      case multiple => PortfolioSolverFactory(p) {
        multiple.map(name => getFromName(name)(p, ctx)(enc))
      }
    }
  }

  def getFromSettings(p: Program, ctx: inox.Context)
                     (enc: inox.ast.ProgramTransformer {
                        val sourceProgram: p.type
                        val targetProgram: StainlessProgram
                      })(implicit sem: p.Semantics): SolverFactory { val program: p.type; type S <: TimeoutSolver { val program: p.type } } = {
    val names = ctx.options.findOptionOrDefault(inox.optSelectedSolvers).toSeq
    getFromNames(names)(p, ctx)(enc)
  }

  def apply(name: String, p: StainlessProgram, ctx: inox.Context): SolverFactory {
    val program: p.type
    type S <: TimeoutSolver { val program: p.type }
  } = getFromName(name)(p, ctx)(inox.ast.ProgramEncoder.empty(p))(p.getSemantics)

  def apply(p: StainlessProgram, ctx: inox.Context): SolverFactory {
    val program: p.type
    type S <: TimeoutSolver { val program: p.type }
  } = getFromSettings(p, ctx)(inox.ast.ProgramEncoder.empty(p))(p.getSemantics)
}
