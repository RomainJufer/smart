/* Copyright 2009-2018 EPFL, Lausanne */

import scala.collection.parallel.ForkJoinTasks
import scala.concurrent.{ ExecutionContext, Future }
import java.util.concurrent.Executors

package object stainless {

  object optJson extends inox.OptionDef[String] {
    val name = "json"
    val default = "report.json"
    val parser = inox.OptionParsers.stringParser
    val usageRhs = "file"
  }

  object optWatch extends inox.FlagOptionDef("watch", false)
  def isWatchModeOn(implicit ctx: inox.Context) = ctx.options.findOptionOrDefault(optWatch)

  object optCompact extends inox.FlagOptionDef("compact", false)
  def isCompactModeOn(implicit ctx: inox.Context) = ctx.options.findOptionOrDefault(optCompact)

  type Program = inox.Program { val trees: ast.Trees }

  type StainlessProgram = Program { val trees: stainless.trees.type }

  /** Including these aliases here makes default imports more natural. */
  type Identifier = inox.Identifier
  val FreshIdentifier = inox.FreshIdentifier

  implicit class IdentifierFromSymbol(id: Identifier) {
    def fullName: String = id match {
      case ast.SymbolIdentifier(name) => name
      case _ => id.name
    }
  }

  object trees extends ast.Trees with inox.ast.SimpleSymbols {
    case class Symbols(
      functions: Map[Identifier, FunDef],
      sorts: Map[Identifier, ADTSort]
    ) extends SimpleSymbols with AbstractSymbols

    object printer extends ast.Printer { val trees: stainless.trees.type = stainless.trees }
  }

  implicit val stainlessSemantics: inox.SemanticsProvider { val trees: stainless.trees.type } =
    new inox.SemanticsProvider {
      val trees: stainless.trees.type = stainless.trees

      def getSemantics(p: inox.Program { val trees: stainless.trees.type }): p.Semantics = new inox.Semantics { self =>
        val trees: p.trees.type = p.trees
        val symbols: p.symbols.type = p.symbols
        val program: Program { val trees: p.trees.type; val symbols: p.symbols.type } =
          p.asInstanceOf[Program { val trees: p.trees.type; val symbols: p.symbols.type }]

        protected def createSolver(ctx: inox.Context): inox.solvers.SolverFactory {
          val program: self.program.type
          type S <: inox.solvers.combinators.TimeoutSolver { val program: self.program.type }
        } = solvers.SolverFactory(self.program, ctx)

        protected def createEvaluator(ctx: inox.Context): inox.evaluators.DeterministicEvaluator {
          val program: self.program.type
        } = evaluators.Evaluator(self.program, ctx)
      }.asInstanceOf[p.Semantics] // @nv: unfortunately required here...
    }

  def encodingSemantics(ts: ast.Trees)
                       (transformer: inox.ast.TreeTransformer { val s: ts.type; val t: stainless.trees.type }):
                        inox.SemanticsProvider { val trees: ts.type } = {
    new inox.SemanticsProvider {
      val trees: ts.type = ts

      def getSemantics(p: inox.Program { val trees: ts.type }): p.Semantics = new inox.Semantics { self =>
        val trees: p.trees.type = p.trees
        val symbols: p.symbols.type = p.symbols
        val program: inox.Program { val trees: p.trees.type; val symbols: p.symbols.type } =
          p.asInstanceOf[inox.Program { val trees: p.trees.type; val symbols: p.symbols.type }]

        private object encoder extends {
          val sourceProgram: self.program.type = self.program
          val t: stainless.trees.type = stainless.trees
        } with inox.ast.ProgramEncoder {
          val encoder = transformer
          object decoder extends ast.TreeTransformer {
            val s: stainless.trees.type = stainless.trees
            val t: trees.type = trees
          }
        }

        protected def createSolver(ctx: inox.Context): inox.solvers.SolverFactory {
          val program: self.program.type
          type S <: inox.solvers.combinators.TimeoutSolver { val program: self.program.type }
        } = solvers.SolverFactory.getFromSettings(self.program, ctx)(encoder)(self.asInstanceOf[self.program.Semantics])

        protected def createEvaluator(ctx: inox.Context): inox.evaluators.DeterministicEvaluator {
          val program: self.program.type
        } = inox.evaluators.EncodingEvaluator(self.program)(encoder)(evaluators.Evaluator(encoder.targetProgram, ctx))
      }.asInstanceOf[p.Semantics] // @nv: unfortunately required here...
    }
  }


  /* Parallelism utilities */

  private lazy val nParallel: Option[Int] =
    Option(System.getProperty("parallel"))
      .flatMap(p => scala.util.Try(p.toInt).toOption)

  lazy val useParallelism: Boolean =
    (nParallel.isEmpty || nParallel.exists(_ > 1)) &&
    !System.getProperty("os.name").toLowerCase().contains("mac")

  private lazy val currentThreadExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutor(new java.util.concurrent.Executor {
      def execute(runnable: Runnable) { runnable.run() }
    })

  private lazy val multiThreadedExecutor: java.util.concurrent.ExecutorService =
    nParallel.map(Executors.newFixedThreadPool(_)).getOrElse(ForkJoinTasks.defaultForkJoinPool)
  private lazy val multiThreadedExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutor(multiThreadedExecutor)

  implicit def executionContext(implicit ctx: inox.Context): ExecutionContext =
    if (useParallelism && ctx.reporter.debugSections.isEmpty) multiThreadedExecutionContext
    else currentThreadExecutionContext

  def shutdown(): Unit = if (useParallelism) multiThreadedExecutor.shutdown()
}
