/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package methods

trait Trees extends throwing.Trees { self =>

  override protected def unapplyScrut(scrut: Expr, up: UnapplyPattern)(implicit s: Symbols): Expr =
    if (s.lookupFunction(up.id).exists(_.flags.exists { case IsMethodOf(_) => true case _ => false }) && up.recs.size == 1) {
      MethodInvocation(up.recs.head, up.id, up.tps, Seq(scrut))
    } else {
      super.unapplyScrut(scrut, up)
    }

  override protected def unapplyAccessor(unapplied: Expr, id: Identifier, up: UnapplyPattern)(implicit s: Symbols): Expr =
    if (s.lookupFunction(id).exists(_.flags.exists { case IsMethodOf(_) => true case _ => false })) {
      MethodInvocation(unapplied, id, Seq(), Seq())
    } else {
      super.unapplyAccessor(unapplied, id, up)
    }

  /** $encodingof `this` */
  case class This(ct: ClassType) extends Expr with Terminal {
    def getType(implicit s: Symbols): Type = ct.getType
  }

  /** $encodingof `super` */
  case class Super(ct: ClassType) extends Expr with Terminal {
    def getType(implicit s: Symbols): Type = ct.getType
  }

  /** $encodingof `receiver.id[tps](args)` */
  case class MethodInvocation(receiver: Expr, id: Identifier, tps: Seq[Type], args: Seq[Expr]) extends Expr with CachingTyped {
    protected def computeType(implicit s: Symbols): Type = widen(receiver.getType) match {
      case ct: ClassType =>
        val optTfd = s.lookupFunction(id)
          .filter(fd => tps.size == fd.tparams.size && args.size == fd.params.size)
          .map(_.typed(tps))
        val optTcd = s.lookupClass(ct.id)
          .filter(cd => ct.tps.size == cd.tparams.size)
          .map(_.typed(ct.tps))

        (optTfd zip optTcd).headOption.flatMap { case (tfd, tcd) =>
          tfd.fd.flags.collectFirst { case IsMethodOf(cid) => cid }
            .flatMap(cid => (tcd +: tcd.ancestors).find(_.id == cid))
            .map { tcd =>
              val tpSubst = tcd.tpSubst ++ tfd.tpSubst
              checkParamTypes(
                args,
                tfd.fd.params.map(vd => typeOps.instantiateType(vd.getType, tpSubst)),
                typeOps.instantiateType(tfd.fd.getType, tpSubst)
              )
            }
        }.getOrElse(Untyped)

      case _ => Untyped
    }
  }


  type Symbols >: Null <: AbstractSymbols

  trait AbstractSymbols
    extends super.AbstractSymbols
       with DependencyGraph
       with TypeOps { self0: Symbols =>
  }


  case class IsAccessor(id: Option[Identifier]) extends Flag("accessor", id.toSeq)
  case class IsMethodOf(id: Identifier) extends Flag("method", Seq(id))

  implicit class ClassDefWrapper(cd: ClassDef) {
    def methods(implicit s: Symbols): Seq[SymbolIdentifier] = {
      s.functions.values.filter(_.flags contains IsMethodOf(cd.id)).map(_.id.asInstanceOf[SymbolIdentifier]).toSeq
    }

    def invariant(implicit s: Symbols): Option[FunDef] = {
      methods map s.functions find (_.flags contains IsInvariant)
    }
  }

  override def getDeconstructor(that: inox.ast.Trees): inox.ast.TreeDeconstructor { val s: self.type; val t: that.type } = that match {
    case tree: Trees => new TreeDeconstructor {
      protected val s: self.type = self
      protected val t: tree.type = tree
    }.asInstanceOf[TreeDeconstructor { val s: self.type; val t: that.type }]

    case _ => super.getDeconstructor(that)
  }
}

trait Printer extends throwing.Printer {
  protected val trees: Trees
  import trees._

  override def ppBody(tree: Tree)(implicit ctx: PrinterContext): Unit = tree match {
    case cd: ClassDef =>
      super.ppBody(cd)
      ctx.opts.symbols.foreach { implicit s =>
        if (cd.methods.nonEmpty) {
          p""" {
            |  ${functions(cd.methods)}
          |}"""
        }
      }

    case MethodInvocation(caller, id, tps, args) =>
      p"$caller.$id${nary(tps, ", ", "[", "]")}"
      if (args.nonEmpty) {
        // TODO: handle implicit arguments and/or default values
        p"($args)"
      }

    case This(_) => p"this"

    case Super(_) => p"super"

    case _ => super.ppBody(tree)
  }

  override protected def requiresParentheses(ex: Tree, within: Option[Tree]): Boolean = (ex, within) match {
    case (_, Some(MethodInvocation(_, _, _, args))) => !args.contains(ex)
    case _ => super.requiresParentheses(ex, within)
  }
}

trait TreeDeconstructor extends throwing.TreeDeconstructor {
  protected val s: Trees
  protected val t: Trees

  override def deconstruct(e: s.Expr): Deconstructed[t.Expr] = e match {
    case s.MethodInvocation(rec, id, tps, args) =>
      (Seq(id), Seq(), rec +: args, tps, Seq(), (ids, _, es, tps, _) => t.MethodInvocation(es(0), ids.head, tps, es.tail))

    case s.This(ct) =>
      (Seq(), Seq(), Seq(), Seq(ct), Seq(), (_, _, _, tps, _) => t.This(tps.head.asInstanceOf[t.ClassType]))

    case s.Super(ct) =>
      (Seq(), Seq(), Seq(), Seq(ct), Seq(), (_, _, _, tps, _) => t.Super(tps.head.asInstanceOf[t.ClassType]))

    case _ => super.deconstruct(e)
  }

  override def deconstruct(f: s.Flag): DeconstructedFlag = f match {
    case s.IsMethodOf(id) => (Seq(id), Seq(), Seq(), (ids, _, _) => t.IsMethodOf(ids.head))
    case s.IsAccessor(id) => (id.toSeq, Seq(), Seq(), (ids, _, _) => t.IsAccessor(ids.headOption))
    case _ => super.deconstruct(f)
  }
}
