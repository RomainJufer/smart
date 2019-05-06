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
    val addressType = symbols.lookup[ClassDef]("stainless.smartcontracts.Address").typed.toType
    val addressAccesorId = symbols.lookup[FunDef]("stainless.smartcontracts.ContractInterface.addr").id

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
      (cd.id, inv)
    }.toMap

    val invariants = existingInvariants ++ implicitInvariant.map { case (id, inv) => id -> inv.id}.toMap

    val contractInvariant = symbols.classes.values.filter(_.isContract).map( cd => {
      val envVd = ValDef.fresh("env", envType)
      val envVar = envVd.toVariable

      val invariantCall = MethodInvocation(This(cd.typed.toType), invariants(cd.id), Seq(), Seq(envVar))

      val addressFields = cd.methods.map(symbols.functions).filter{ case fd => fd.returnType == addressType }

      val addressInvariants = addressFields.map( fd => {
        val addressOfFlagOpt = fd.flags.collectFirst{ case AddressOfContract(name) => name }
        if(addressOfFlagOpt == None)
          BooleanLiteral(true)
        else {
          val addressOfFlag = addressOfFlagOpt.get
          val addressContract = contracts.collectFirst{ case cd if cd.id.name contains addressOfFlag => cd}.get

          val addrCall = MethodInvocation(This(cd.typed.toType), fd.id, Seq(), Seq())
          val addrEquality = Equals(addrCall, 
            MethodInvocation(  
              AsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), addrCall), addressContract.typed.toType),
              addressAccesorId, Seq(), Seq()))

          And(IsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), addrCall), addressContract.typed.toType),
            And(MethodInvocation(
              AsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtAccessor), addrCall), addressContract.typed.toType),
              invariants(addressContract.id), Seq(), Seq(envVar)), addrEquality))
        }
      }).foldLeft[Expr](invariantCall)(And(_, _))

      val invDef = new FunDef(
        ast.SymbolIdentifier("contractInvariant"),
        Seq(),
        Seq(envVd),
        BooleanType(),
        addressInvariants,
        Seq(Synthetic, IsPure, Final, IsMethodOf(cd.id))
      )
      
      (cd.id, invDef)
    }).toMap

    override def transform(fd: FunDef): FunDef = fd match {
      case fd if fd.isContractMethod =>
        val contract = symbols.classes(fd.findClass.get)
        val contractType = contract.typed.toType

        val envVar = fd.params.collectFirst{
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        val currPre = preconditionOf(fd.fullBody).getOrElse(BooleanLiteral(true))
        val Lambda(vds, currPost) = postconditionOf(fd.fullBody).getOrElse(Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true)))

        val invCall = MethodInvocation(This(contractType), contractInvariant(contract.id).id, Seq(), Seq(envVar))
        val newPre = Precondition(And(invCall, currPre))
        val newPost = Postcondition(Lambda(vds, And(invCall, currPost)))

        super.transform(fd.copy(
          fullBody = reconstructSpecs(Seq(newPre, newPost), withoutSpecs(fd.fullBody), fd.returnType)
        ).copiedFrom(fd))

      case fd => super.transform(fd)
    }

    val newFuns = implicitInvariant.values.toSeq ++ contractInvariant.values.toSeq
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
