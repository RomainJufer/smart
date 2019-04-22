/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait GlobalInvariantInjection extends oo.SimplePhase
  with oo.SimplyCachedClasses
  with SimplyCachedSorts
  with SimplyCachedFunctions
  { self =>
  val s: trees.type
  val t: s.type
  import s._

  /* ====================================
   *       Context and caches setup
   * ==================================== */

  override protected def getContext(symbols: s.Symbols) = new TransformerContext()(symbols)
  protected class TransformerContext(implicit val symbols: s.Symbols) extends oo.TreeTransformer {
    val s: self.s.type = self.s
    val t: self.t.type = self.t

    import s.exprOps._

    val envCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment")
    val envType: ClassType = envCd.typed.toType
    val contractAtAccessor: Identifier = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get.id
    val contractInterfaceCd: ClassDef = symbols.lookup[ClassDef]("stainless.smartcontracts.ContractInterface")
    val contractInterfaceType: ClassType = contractInterfaceCd.typed.toType

    def checkInvariantForm(cid: Identifier, fd: FunDef) = {
      if (
        fd.typeArgs.isEmpty &&
        fd.params.forall(p => p.getType == envType) &&
        fd.returnType == BooleanType()
      ) ()
      else {
        context.reporter.fatalError(s"The `invariant` function of contract ${cid.asString} must be of type: invariant(): Boolean")
      }
    }

    // We collect all contracts
    val contracts = symbols.classes.values.filter(_.isContract)
    val existingInvariants: Map[Identifier, Identifier] = contracts.map { cd =>
      symbols.functions.values.collectFirst {
        case fd if (fd.isInClass(cd.id) && fd.id.name == "invariant") =>
          checkInvariantForm(cd.id, fd)
          context.reporter.info(s"Found invariant for ${cd.id.asString}:\n${fd.fullBody.asString}")
          (cd.id, fd.id)
      }
    }.flatten.toMap

    val implicitInvariant = contracts.filterNot(c => existingInvariants.contains(c.id)).map { case cd =>
      context.reporter.info(s"No invariant was found for contract ${cd.id}. Implicit invariant() = true has been generated")
      val inv = new FunDef(
        ast.SymbolIdentifier("invariant"),
        Seq(),
        Seq(ValDef.fresh("env", envType)),
        BooleanType(),
        BooleanLiteral(true),
        Seq(Synthetic, IsPure, Final, IsMethodOf(cd.id))
      )
      (cd, inv)
    }

    val invariants = existingInvariants ++ implicitInvariant.map{ case (cd, inv) => cd.id -> inv.id }.toMap

    // We make sure that contracts are not extended
    contracts.find { cd => !cd.children.isEmpty } match {
      case Some(cd) => context.reporter.fatalError("A contract cannot be extended: ${cd.id.asString}.")
      case None => ()
    }

    val addressOfMap = contracts.map{ case cd =>
      cd.id -> symbols.lookup[FunDef](s"addressOf${cd.id}")
    }.toMap

    val envGIEnv = ValDef.fresh("env", envType)

    /*def buildAddressOfDiffExpr(l: Seq[FunDef]):Expr = l match {
      case Nil => BooleanLiteral(true)
      case x :: xs =>
        val eqs = xs.map( fd => Not(Equals(FunctionInvocation(x.id, Nil, Nil), FunctionInvocation(fd.id, Nil, Nil))))
        if(eqs.isEmpty)
          buildAddressOfDiffExpr(xs)
        else
          eqs.reduce[Expr]( And(_, _))
    }*/

    //val giAddressOfDiffExpr = buildAddressOfDiffExpr(addressOfs)

    val giFoundation = (envTarget:Expr) => contracts.map{ case contract => 
      val contractType = contract.typed.toType
      val addressOf = symbols.lookup[FunDef](s"addressOf${contract.id}")
      IsInstanceOf(
          MutableMapApply(
            ClassSelector(envTarget, contractAtAccessor),
              FunctionInvocation(addressOf.id, Nil, Nil)),
          contractType)
    }.foldLeft[Expr](BooleanLiteral(true))(And(_, _))

    val giLocalInvariantCall = contracts.map{ case contract =>
      val contractType = contract.typed.toType
      val addressOf = symbols.lookup[FunDef](s"addressOf${contract.id}")
        MethodInvocation(
          AsInstanceOf(
            MutableMapApply(
              ClassSelector(This(envCd.typed.toType), contractAtAccessor),
                FunctionInvocation(addressOf.id, Nil, Nil)),
            contractType),
          invariants(contract.id),
          Seq(),
          Seq(This(envType)))
    }.foldLeft[Expr](BooleanLiteral(true))(And(_, _))


    val environmentInvariant = new FunDef (
      ast.SymbolIdentifier("invariant"),
      Seq(),
      Seq(),
      BooleanType(),
      And(giFoundation(This(envCd.typed.toType)), giLocalInvariantCall),
      Seq(Synthetic, Final, Ghost, IsMethodOf(envCd.id))
    )

    context.reporter.info(s"Environment invariant : \n${environmentInvariant.fullBody}")

    override def transform(fd: FunDef): FunDef = {
      if(fd.isContractMethod) {
        val contract = fd.findClass.get

        val envVar = fd.params.collectFirst{
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        val currentPre:Expr = preconditionOf(fd.fullBody).getOrElse(BooleanLiteral(true))
        val Lambda(vds, bdy) = postconditionOf(fd.fullBody).getOrElse(Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true)))

        val contractId = fd.flags.collectFirst{ case IsMethodOf(id) => id}.get

        val newBody = postMap {
          case FunctionInvocation(id, Seq(), Seq()) if isIdentifier("stainless.smartcontracts.Environment.invariant", id) =>
            Some(MethodInvocation(envVar, environmentInvariant.id, Seq(), Seq()))

          case _ => None
        }(withoutSpecs(fd.fullBody).getOrElse(NoTree(UnitType())))

        val callEnvInvariant = MethodInvocation(envVar, environmentInvariant.id, Seq(), Seq())
        val newPre = if(!fd.isConstructor) Precondition(And(callEnvInvariant, currentPre))
                     else Precondition(And(giFoundation(envVar), currentPre))
        val newPost = Postcondition(Lambda(vds, And(callEnvInvariant, bdy)))

        super.transform(fd.copy(fullBody = reconstructSpecs(Seq(newPre, newPost), Some(newBody), fd.returnType))
                                .copiedFrom(fd))

      } else {
        super.transform(fd)
      }
    }

    /*override def transform(e: Expr): Expr = e match {
      case MethodInvocation(This(_), invariantId, Seq(), Seq(env))
          if existingInvariants.values.toSeq.contains(invariantId) =>
        FunctionInvocation(transform(invariantId), Seq(), Seq(transform(env)))
      case _ => super.transform(e)
    }*/

    val newFuns = Seq(environmentInvariant) ++
                  implicitInvariant.map{ case (cd, inv) => inv }
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    super.extractSymbols(context, symbols.withFunctions(context.newFuns.toSeq))
  }
}

object GlobalInvariantInjection {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with GlobalInvariantInjection
}
