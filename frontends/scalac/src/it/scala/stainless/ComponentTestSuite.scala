/* Copyright 2009-2018 EPFL, Lausanne */

package stainless

import scala.concurrent.Await
import scala.concurrent.duration._

trait ComponentTestSuite extends inox.TestSuite with inox.ResourceUtils with InputUtils { self =>

  val component: Component

  override def configurations: Seq[Seq[inox.OptionValue[_]]] = Seq(
    Seq(inox.optSelectedSolvers(Set("smt-z3")), inox.optTimeout(300.seconds))
  )

  // final override def createContext(options: inox.Options) = stainless.TestContext.debug(inox.solvers.DebugSectionSolver, options)
  final override def createContext(options: inox.Options) = stainless.TestContext(options)

  override protected def optionsString(options: inox.Options): String = {
    "solvr=" + options.findOptionOrDefault(inox.optSelectedSolvers).head + " " +
    "lucky=" + options.findOptionOrDefault(inox.solvers.unrolling.optFeelingLucky) + " " +
    "check=" + options.findOptionOrDefault(inox.solvers.optCheckModels)
  }

  protected def filter(ctx: inox.Context, name: String): FilterStatus = Test

  def testAll(dir: String, subdirectories: Boolean = false)(block: (component.Analysis, inox.Reporter) => Unit): Unit = {
    val fs = resourceFiles(dir, _.endsWith(".scala"), recursive = subdirectories).toList

    // Toggle this variable if you need to debug one specific test.
    // You might also want to run `it:testOnly *<some test suite>* -- -z "<some test filter>"`.
    val DEBUG = false

    if (DEBUG) {
      for {
        file <- fs.sortBy(_.getPath)
        path = file.getPath
        name = file.getName dropRight ".scala".length
      } test(s"$dir/$name", ctx => filter(ctx, s"$dir/$name")) { implicit ctx =>
        val (structure, program) = loadFiles(Seq(path))
        assert((structure count { _.isMain }) == 1, "Expecting only one main unit")
        program.symbols.ensureWellFormed

        val run = component.run(extraction.pipeline)

        val exProgram = inox.Program(run.trees)(run extract program.symbols)
        exProgram.symbols.ensureWellFormed
        assert(ctx.reporter.errorCount == 0)

        val unit = structure.find { _.isMain }.get
        assert(unit.id.name == name, "Expecting compilation unit to have same name as source file")

        val defs = inox.utils.fixpoint { (defs: Set[Identifier]) =>
          def derived(flags: Seq[run.trees.Flag]): Boolean =
            (defs & flags.collect { case run.trees.Derived(id) => id }.toSet).nonEmpty

          defs ++
          exProgram.symbols.functions.values.filter(fd => derived(fd.flags)).map(_.id) ++
          exProgram.symbols.sorts.values.filter(sort => derived(sort.flags)).map(_.id)
        } (unit.allFunctions(program.symbols).toSet ++ unit.allClasses)

        val funs = defs.filter(exProgram.symbols.functions contains _).toSeq

        val report = Await.result(run.apply(funs, exProgram.symbols), Duration.Inf)
        block(report, ctx.reporter)
      }
    } else {
      implicit val ctx = inox.TestContext.empty
      val (structure, program) = loadFiles(fs.map(_.getPath))
      program.symbols.ensureWellFormed

      // We use a shared run during extraction to ensure caching of
      // extraction results is enabled.
      val extractor = component.run(extraction.pipeline)

      val solidityIds = program.symbols.classes.values.filter(extraction.smartcontracts.isSmartContractDep).map(_.id) ++
                        program.symbols.functions.values.filter(extraction.smartcontracts.isSmartContractDep).map(_.id)
      val solidityDeps = solidityIds.flatMap(id => program.symbols.dependencies(id) + id)

      for {
        unit <- structure
        if unit.isMain
        name = unit.id.name
      } test(s"$dir/$name", ctx => filter(ctx, s"$dir/$name")) { implicit ctx =>
        val defs = unit.allFunctions(program.symbols).toSet ++ unit.allClasses


        val deps = defs.flatMap(id => program.symbols.dependencies(id) + id) ++ solidityDeps

        val symbols = extraction.xlang.trees.NoSymbols
          .withClasses(program.symbols.classes.values.filter(cd => deps(cd.id)).toSeq)
          .withFunctions(program.symbols.functions.values.filter(fd => deps(fd.id)).toSeq)

        val exSymbols = extractor extract symbols
        exSymbols.ensureWellFormed

        val run = component.run(extraction.pipeline)

        val funs = inox.utils.fixpoint { (defs: Set[Identifier]) =>
          def derived(flags: Seq[extractor.trees.Flag]): Boolean =
            (defs & flags.collect { case extractor.trees.Derived(id) => id }.toSet).nonEmpty

          defs ++
          exSymbols.functions.values.filter(fd => derived(fd.flags)).map(_.id) ++
          exSymbols.sorts.values.filter(sort => derived(sort.flags)).map(_.id)
        } (defs).toSeq.filter(exSymbols.functions contains _)
        .filterNot{fd =>
          symbols.functions.isDefinedAt(fd) && (
            symbols.functions(fd).flags.exists {_.name == "library"} ||
            symbols.functions(fd).flags.exists {_.name == "synthetic"}
          )
        }

        // We have to cast the extracted symbols type as we are using two different
        // run instances. However, the trees types are the same so this should be safe (tm).
        val report = Await.result(
          run.apply(funs, exSymbols.asInstanceOf[run.trees.Symbols]),
          Duration.Inf
        )

        block(report, ctx.reporter)
      }
    }
  }

}

