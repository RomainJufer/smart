/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait ContractMethodLifting extends oo.SimplePhase
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

    val addressCd = symbols.lookup[ClassDef]("stainless.smartcontracts.Address")
    val addressType = addressCd.typed.toType
    val envCd = symbols.lookup[ClassDef]("stainless.smartcontracts.Environment")
    val envType = envCd.typed.toType
    val contractAtId = envCd.fields.find(vd => isIdentifier("stainless.smartcontracts.Environment.contractAt", vd.id)).get.id

    def processBody(body: Expr, env: Variable, callee: Variable): Expr = {
      postMap {
        case This(tp) => 
          Some(AsInstanceOf(MutableMapApply(ClassSelector(env, contractAtId), callee), tp))

        case MethodInvocation(This(receiverType), id, tps, args) if symbols.functions(id).isContractMethod || symbols.functions(id).isInvariant =>
          Some(FunctionInvocation(id, tps, args :+ callee))

        case MethodInvocation(receiver, id, tps, args) if symbols.functions(id).isContractMethod || symbols.functions(id).isInvariant =>
          val AsInstanceOf(MutableMapApply(ClassSelector(_, _), calleeAddr), _) = receiver 
          Some(FunctionInvocation(id, tps, args ++ Seq(calleeAddr)))

        case _ => None
      }(body)
    }

    override def transform(fd: FunDef): FunDef = fd match {
      case fd if fd.isContractMethod || fd.isInvariant =>
        val contract = symbols.classes(fd.findClass.get)
        val contractType = contract.typed.toType

        val envVar = fd.params.collectFirst{
          case v@ValDef(_, tpe, _) if tpe == envType => v.toVariable
        }.get

        val calleeVd = ValDef.fresh("callee", addressType)
        val newParams = fd.params :+ calleeVd

        val newBody = processBody(fd.fullBody, envVar, calleeVd.toVariable)

        val calleeIsInstanceOf = IsInstanceOf(MutableMapApply(ClassSelector(envVar, contractAtId), calleeVd.toVariable), contractType)
        val currPre = preconditionOf(newBody).getOrElse(BooleanLiteral(true))
        val newPre = Precondition(And(calleeIsInstanceOf, currPre))

        val Lambda(vds, body) = postconditionOf(newBody).getOrElse(Lambda(Seq(ValDef.fresh("res", fd.returnType)), BooleanLiteral(true)))
        val newPost = Postcondition(Lambda(vds, And(calleeIsInstanceOf, body)))

        super.transform(fd.copy(
          flags = fd.flags.filterNot{ case IsMethodOf(_) => true case _ => false},
          params = newParams,
          fullBody = reconstructSpecs(Seq(newPre, newPost), Some(newBody), fd.returnType)
        ).copiedFrom(fd))

      case fd => super.transform(fd)
    }
  }

  /* ====================================
   *             Extraction
   * ==================================== */

  override def extractSymbols(context: TransformerContext, symbols: Symbols): Symbols = {
    super.extractSymbols(context, symbols)
  }
}

object ContractMethodLifting {
  def apply()(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  } with ContractMethodLifting
}
