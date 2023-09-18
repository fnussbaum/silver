// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

package viper.silver.parser

import viper.silver.FastMessaging
import viper.silver.ast.utility._
import viper.silver.ast.{SourcePosition, _}
import viper.silver.plugin.standard.adt.{Adt, AdtType}

import scala.language.implicitConversions

/**
 * Takes an abstract syntax tree after parsing is done and translates it into
 * a Viper abstract syntax tree.
 *
 * [2014-05-08 Malte] The current architecture of the resolver makes it hard
 * to detect all malformed ASTs. It is, for example, hard to detect that an
 * expression "f > 0", where f is an int-typed field, is malformed.
 * The translator can thus not assume that the input tree is completely
 * wellformed, and in cases where a malformed tree is detected, it does not
 * return a tree, but instead, records error messages using the
 * Messaging feature.
 */
case class Translator(program: PProgram) {
  def translate: Option[Program] /*(Program, Seq[Messaging.Record])*/ = {
    // assert(TypeChecker.messagecount == 0, "Expected previous phases to succeed, but found error messages.") // AS: no longer sharing state with these phases

    program match {
      case PProgram(_, _, pdomains, pfields, pfunctions, ppredicates, pmethods, pextensions, _) =>

        /* [2022-03-14 Alessandro] Domain signatures need no be translated first, since signatures of other declarations
         * like domain functions, and ordinary functions might depend on the domain signature. Especially this is the case
         * when signatures contain user-defined domain types. The same applies for extensions since they might introduce
         * new top-level declarations that behave similar as domains.
         */
        pdomains foreach translateMemberSignature
        pextensions foreach translateMemberSignature

        /* [2022-03-14 Alessandro] Following signatures can be translated independently of each other but must be translated
         * after signatures of domains and extensions because of the above mentioned reasons.
         */
        pdomains flatMap (_.members.funcs) foreach translateMemberSignature
        (pfields ++ pfunctions ++ ppredicates ++ pmethods) foreach translateMemberSignature

        /* [2022-03-14 Alessandro] After the signatures are translated, the actual full translations can be done
         * independently of each other.
         */
        val extensions = pextensions map translate
        val domain = (pdomains map translate) ++ extensions filter (t => t.isInstanceOf[Domain])
        val fields = (pfields flatMap (_.fields map translate)) ++ extensions filter (t => t.isInstanceOf[Field])
        val functions = (pfunctions map translate) ++ extensions filter (t => t.isInstanceOf[Function])
        val predicates = (ppredicates map translate) ++ extensions filter (t => t.isInstanceOf[Predicate])
        val methods = (pmethods map translate) ++ extensions filter (t => t.isInstanceOf[Method])



        val finalProgram = ImpureAssumeRewriter.rewriteAssumes(Program(domain.asInstanceOf[Seq[Domain]], fields.asInstanceOf[Seq[Field]],
                functions.asInstanceOf[Seq[Function]], predicates.asInstanceOf[Seq[Predicate]], methods.asInstanceOf[Seq[Method]],
                    (extensions filter (t => t.isInstanceOf[ExtensionMember])).asInstanceOf[Seq[ExtensionMember]])(program))

        finalProgram.deepCollect {
          case fp: ForPerm => Consistency.checkForPermArguments(fp, finalProgram)
          case trig: Trigger => Consistency.checkTriggers(trig, finalProgram)
        }

        if (Consistency.messages.isEmpty) Some(finalProgram) // all error messages generated during translation should be Consistency messages
        else None
    }
  }

  private def translate(t: PExtender): Member = {
    t.translateMember(this)
  }

  private def translate(m: PMethod): Method = m match {
    case PMethod(_, _, name, _, _, _, pres, posts, body) =>
      val m = findMethod(name)

      val newBody = body.map(actualBody => {
        val b = stmt(actualBody).asInstanceOf[Seqn]
        val newScopedDecls = b.scopedSeqnDeclarations ++ b.deepCollect {case l: Label => l}

        b.copy(scopedSeqnDeclarations = newScopedDecls)(b.pos, b.info, b.errT)
      })

      val finalMethod = m.copy(pres = pres map (p => exp(p._2)), posts = posts map (p => exp(p._2)), body = newBody)(m.pos, m.info, m.errT)

      members(m.name) = finalMethod

      finalMethod
  }

  private def translate(d: PDomain): Domain = d match {
    case PDomain(_, _, name, _, PDomainMembers(functions, axioms), interpretation) =>
      val d = findDomain(name)
      val dd = d.copy(functions = functions map (f => findDomainFunction(f.idndef)),
        axioms = axioms map translate, interpretations = interpretation.map(_.interps))(d.pos, d.info, d.errT)
      members(d.name) = dd
      dd
  }

  private def translate(a: PAxiom): DomainAxiom = a match {
    case pa@PAxiom(anns, _, Some(name), e) =>
      NamedDomainAxiom(name.name, exp(e.inner))(a, Translator.toInfo(anns, pa), domainName = pa.domainName.name)
    case pa@PAxiom(anns, _, None, e) =>
      AnonymousDomainAxiom(exp(e.inner))(a, Translator.toInfo(anns, pa), domainName = pa.domainName.name)
  }

  private def translate(f: PFunction): Function = f match {
    case PFunction(_, _, name, _, _, pres, posts, body) =>
      val f = findFunction(name)
      val ff = f.copy( pres = pres map (p => exp(p._2)), posts = posts map (p => exp(p._2)), body = body map (_.inner) map exp)(f.pos, f.info, f.errT)
      members(f.name) = ff
      ff
  }

  private def translate(p: PPredicate): Predicate = p match {
    case PPredicate(_, _, name, _, body) =>
      val p = findPredicate(name)
      val pp = p.copy(body = body map (_.inner) map exp)(p.pos, p.info, p.errT)
      members(p.name) = pp
      pp
  }

  private def translate(f: PFieldDecl) = findField(f.idndef)

  private val members = collection.mutable.HashMap[String, Node]()
  def getMembers() = members
  /**
    * Translate the signature of a member, so that it can be looked up later.
    *
    * TODO: Get rid of this method!
    *         - Passing lots of null references is just asking for trouble
    *         - It should no longer be necessary to have this lookup table because, e.g. a
    *           method call no longer needs the method node, the method name (as a string)
    *           suffices
    */
  private def translateMemberSignature(p: PMember): Unit = p.declares foreach { decl =>
    val pos = decl
    val name = decl.idndef.name
    val t = decl match {
      case pf@PFieldDecl(_, typ) =>
        Field(name, ttyp(typ))(pos, Translator.toInfo(p.annotations, pf))
      case pf@PFunction(_, _, _, formalArgs, typ, _, _, _) =>
        Function(name, formalArgs map liftArgDecl, ttyp(typ), null, null, null)(pos, Translator.toInfo(p.annotations, pf))
      case pdf@PDomainFunction(_, unique, _, _, args, typ, interp) =>
        DomainFunc(name, args map liftAnyArgDecl, ttyp(typ), unique.isDefined, interp.map(_._2.s))(pos, Translator.toInfo(p.annotations, pdf),pdf.domainName.name)
      case pd@PDomain(_, _, _, typVars, _, interp) =>
        Domain(name, null, null, typVars map (t => TypeVar(t.idndef.name)), interp.map(_.interps))(pos, Translator.toInfo(p.annotations, pd))
      case pp@PPredicate(_, _, _, formalArgs, _) =>
        Predicate(name, formalArgs map liftArgDecl, null)(pos, Translator.toInfo(p.annotations, pp))
      case pm@PMethod(_, _, _, formalArgs, _, formalReturns, _, _, _) =>
        Method(name, formalArgs map liftArgDecl, formalReturns map liftReturnDecl, null, null, null)(pos, Translator.toInfo(p.annotations, pm))
    }
    members.put(decl.idndef.name, t)
  }

  private def translateMemberSignature(p: PExtender): Unit ={
    p match {
      case _: PMember =>
        val l = p.translateMemberSignature(this)
        members.put(l.name, l)
    }
  }

  // helper methods that can be called if one knows what 'id' refers to
  private def findDomain(id: PIdentifier) = members(id.name).asInstanceOf[Domain]
  private def findField(id: PIdentifier) = members(id.name).asInstanceOf[Field]
  private def findFunction(id: PIdentifier) = members(id.name).asInstanceOf[Function]
  private def findDomainFunction(id: PIdentifier) = members(id.name).asInstanceOf[DomainFunc]
  private def findPredicate(id: PIdentifier) = members(id.name).asInstanceOf[Predicate]
  private def findMethod(id: PIdentifier) = members(id.name).asInstanceOf[Method]

  /** Takes a `PStmt` and turns it into a `Stmt`. */
  def stmt(pStmt: PStmt): Stmt = {
    val pos = pStmt
    val (s, annotations) = extractAnnotationFromStmt(pStmt)
    val info = if (annotations.isEmpty) NoInfo else AnnotationInfo(annotations)
    val subInfo = NoInfo
    s match {
      case PAssign(targets, _, PCall(method, _, args, _, _)) if members(method.name).isInstanceOf[Method] =>
        methodCallAssign(s, targets, ts => MethodCall(findMethod(method), args map exp, ts)(pos, info))
      case PAssign(targets, _, _) if targets.length != 1 =>
        sys.error(s"Found non-unary target of assignment")
      case PAssign(Seq(target), _, PNewExp(_, fieldsOpt)) =>
        val fields = fieldsOpt match {
          // Note that this will not use any fields that extensions declare
          case None => program.fields flatMap (_.fields map translate)
          case Some(pfields) => pfields map findField
        }
        methodCallAssign(s, Seq(target), lv => NewStmt(lv.head, fields)(pos, info))
      case PAssign(Seq(idnuse: PIdnUse), _, rhs) =>
        LocalVarAssign(LocalVar(idnuse.name, ttyp(idnuse.typ))(pos, subInfo), exp(rhs))(pos, info)
      case PAssign(Seq(field: PFieldAccess), _, rhs) =>
        FieldAssign(FieldAccess(exp(field.rcv), findField(field.idnuse))(field), exp(rhs))(pos, info)
      case lv@PVars(_, vars, init) =>
        // there are no declarations in the Viper AST; rather they are part of the scope signature
        init match {
          case Some(assign) =>
            val tgts = vars.map(_.toIdnUse)
            stmt(PAssign(tgts, Some(assign._1), assign._2)(lv.pos))
          case None => Statements.EmptyStmt
        }
      case PSeqn(ss) =>
        val plocals = ss.collect {
          case l: PVars => Some(l)
          case _ => None
        }
        val locals = plocals.flatten.map {
          case p@PVars(_, vars, _) => vars.map(v => LocalVarDecl(v.idndef.name, ttyp(v.typ))(p))
        }.flatten
        Seqn(ss filterNot (_.isInstanceOf[PSkip]) map stmt, locals)(pos, info)
      case PFold(_, e) =>
        Fold(exp(e).asInstanceOf[PredicateAccessPredicate])(pos, info)
      case PUnfold(_, e) =>
        Unfold(exp(e).asInstanceOf[PredicateAccessPredicate])(pos, info)
      case PPackageWand(_, e, proofScript) =>
        val wand = exp(e).asInstanceOf[MagicWand]
        Package(wand, stmt(proofScript).asInstanceOf[Seqn])(pos, info)
      case PApplyWand(_, e) =>
        Apply(exp(e).asInstanceOf[MagicWand])(pos, info)
      case PInhale(_, e) =>
        Inhale(exp(e))(pos, info)
      case PAssume(_, e) =>
        Assume(exp(e))(pos, info)
      case PExhale(_, e) =>
        Exhale(exp(e))(pos, info)
      case PAssert(_, e) =>
        Assert(exp(e))(pos, info)
      case PLabel(_, name, invs) =>
        Label(name.name, invs map (_._2) map exp)(pos, info)
      case PGoto(_, label) =>
        Goto(label.name)(pos, info)
      case PIf(_, cond, thn, _, els) =>
        If(exp(cond), stmt(thn).asInstanceOf[Seqn], stmt(els).asInstanceOf[Seqn])(pos, info)
      case PWhile(_, cond, invs, body) =>
        While(exp(cond), invs map (inv => exp(inv._2)), stmt(body).asInstanceOf[Seqn])(pos, info)
      case PQuasihavoc(_, lhs, e) =>
        val (newLhs, newE) = havocStmtHelper(lhs, e)
        Quasihavoc(newLhs, newE)(pos, info)
      case PQuasihavocall(_, vars, _, lhs, e) =>
        val newVars = vars map liftLogicalDecl
        val (newLhs, newE) = havocStmtHelper(lhs, e)
        Quasihavocall(newVars, newLhs, newE)(pos, info)
      case t: PExtender =>   t.translateStmt(this)
      case _: PDefine | _: PSkip =>
        sys.error(s"Found unexpected intermediate statement $s (${s.getClass.getName}})")
    }
  }

  /**
    * Translates a simple PAst `a, b, c := methodCall(...)` to an Ast `a, b, c := methodCall(...)`. But if any
    * targets are field accesses, then the translation is from `(exprA).f, b, (exprC).g := methodCall(...)` to
    * ```
    * {(scopedDecls: _receiver0, _target0, _receiver2, _target2)
    *   _receiver0 := exprA
    *   _receiver2 := exprC
    *   _target0, b, _target2 := methodCall(...)
    *   _receiver0.f := _target0
    *   _receiver2.g := _target2
    * }
    * ```
    */
  def methodCallAssign(errorNode: PNode, targets: Seq[PAssignTarget], assign: Seq[LocalVar] => Stmt): Stmt = {
    val tTargets = targets map exp
    val ts = tTargets.zipWithIndex.map {
      case (lv: LocalVar, _) => (None, lv)
      case (fa: FieldAccess, i) => {
        // --- Before the call ---
        val rcvDecl = LocalVarDecl(s"_receiver$i", fa.rcv.typ)()
        val tgtDecl = LocalVarDecl(s"_target$i", fa.typ)()
        // From the example translation above for the first target the values are:
        // rcvUse: `_receiver0`
        val rcvUse = LocalVar(rcvDecl.name, rcvDecl.typ)(fa.rcv.pos)
        // rcvInit: `_receiver0 := exprA`
        val rcvInit = LocalVarAssign(rcvUse, fa.rcv)(fa.rcv.pos)
        // --- After the call ---
        // tgtUse: `_target0`
        val tgtUse = LocalVar(tgtDecl.name, tgtDecl.typ)(fa.pos)
        // rcvFa: `_receiver0.f`
        val rcvFa = FieldAccess(rcvUse, fa.field)(fa.pos, fa.info, NodeTrafo(fa) + fa.errT)
        // faAssign: `_receiver0.f := _target0`
        val faAssign = FieldAssign(rcvFa, tgtUse)(rcvFa.pos)
        (Some((rcvDecl, tgtDecl, rcvInit, faAssign)), tgtUse)
      }
      case _ => sys.error(s"Found invalid target of assignment")
    }
    val assn = assign(ts.map(_._2))
    val tmps = ts.flatMap(_._1)
    if (tmps.isEmpty)
      return assn
    if (!Consistency.noDuplicates(tmps.map(_._4.lhs.field)))
      Consistency.messages ++= FastMessaging.message(errorNode, s"multiple targets which access the same field are not allowed")
    Seqn(
      tmps.map(_._3) ++
      Seq(assn) ++
      tmps.map(_._4),
      tmps.flatMap(t => Seq(t._1, t._2))
    )(assn.pos, assn.info)
  }

  /** Helper function that translates subexpressions common to a Havoc or Havocall statement */
  def havocStmtHelper(lhs: Option[(PExp, _)], e: PExp): (Option[Exp], ResourceAccess) = {
    val newLhs = lhs.map(lhs => exp(lhs._1))
    exp(e) match {
      case exp: FieldAccess => (newLhs, exp)
      case PredicateAccessPredicate(predAccess, perm) =>
        // A PrediateAccessPredicate is a PredicateResourceAccess combined with
        // a Permission. Havoc expects a ResourceAccess. To make types match,
        // we must extract the PredicateResourceAccess.
        assert(perm.isInstanceOf[FullPerm])
        (newLhs, predAccess)
      case exp: MagicWand => (newLhs, exp)
      case _ => sys.error("Can't havoc this kind of expression")
    }
  }

  def extractAnnotation(pexp: PExp): (PExp, Map[String, Seq[String]]) = {
    pexp match {
      case PAnnotatedExp(ann, e) =>
        val (resPexp, innerMap) = extractAnnotation(e)
        val combinedValue = if (innerMap.contains(ann.key.name)) {
          ann.values.map(_.s) ++ innerMap(ann.key.name)
        } else {
          ann.values.map(_.s)
        }
        (resPexp, innerMap.updated(ann.key.name, combinedValue))
      case _ => (pexp, Map())
    }
  }

  def extractAnnotationFromStmt(pStmt: PStmt): (PStmt, Map[String, Seq[String]]) = {
    pStmt match {
      case PAnnotatedStmt(ann, s) =>
        val (resPStmt, innerMap) = extractAnnotationFromStmt(s)
        val combinedValue = if (innerMap.contains(ann.key.name)) {
          ann.values.map(_.s) ++ innerMap(ann.key.name)
        } else {
          ann.values.map(_.s)
        }
        (resPStmt, innerMap.updated(ann.key.name, combinedValue))
      case _ => (pStmt, Map())
    }
  }

  /** Takes a `PExp` and turns it into an `Exp`. */
  def exp(parseExp: PExp): Exp = {
    val pos = parseExp
    val (pexp, annotationMap) = extractAnnotation(parseExp)
    val info = if (annotationMap.isEmpty) NoInfo else AnnotationInfo(annotationMap)
    pexp match {
      case piu @ PIdnUseExp(name) =>
        piu.decl match {
          case Some(_: PAnyVarDecl) => LocalVar(name, ttyp(pexp.typ))(pos, info)
          // A malformed AST where a field, function or other declaration is used as a variable.
          // Should have been caught by the type checker.
          case _ => sys.error("should not occur in type-checked program")
        }
      case pbe @ PBinExp(left, op, right) =>
        val (l, r) = (exp(left), exp(right))
        op.rs match {
          case PSymOp.Plus =>
            r.typ match {
              case Int => Add(l, r)(pos, info)
              case Perm => PermAdd(l, r)(pos, info)
              case _ => sys.error("should not occur in type-checked program")
            }
          case PSymOp.Minus =>
            r.typ match {
              case Int => Sub(l, r)(pos, info)
              case Perm => PermSub(l, r)(pos, info)
              case _ => sys.error("should not occur in type-checked program")
            }
          case PSymOp.Mul =>
            r.typ match {
              case Int =>
                l.typ match {
                  case Int => Mul(l, r)(pos, info)
                  case Perm => IntPermMul(r, l)(pos, info)
                  case _ => sys.error("should not occur in type-checked program")
                }
              case Perm =>
                l.typ match {
                  case Int => IntPermMul(l, r)(pos, info)
                  case Perm => PermMul(l, r)(pos, info)
                  case _ => sys.error("should not occur in type-checked program")
                }
              case _ => sys.error("should not occur in type-checked program")
            }
          case PSymOp.Div =>
            l.typ match {
              case Perm => r.typ match {
                case Int => PermDiv(l, r)(pos, info)
                case Perm => PermPermDiv(l, r)(pos, info)
              }
              case Int  =>
                assert (r.typ==Int)
                if (ttyp(pbe.typ) == Int)
                  Div(l, r)(pos, info)
                else
                  FractionalPerm(l, r)(pos, info)
              case _    => sys.error("should not occur in type-checked program")
            }
          case PSymOp.ArithDiv => Div(l, r)(pos, info)
          case PSymOp.Mod => Mod(l, r)(pos, info)
          case PSymOp.Lt =>
            l.typ match {
              case Int => LtCmp(l, r)(pos, info)
              case Perm => PermLtCmp(l, r)(pos, info)
              case _ => sys.error("unexpected type")
            }
          case PSymOp.Le =>
            l.typ match {
              case Int => LeCmp(l, r)(pos, info)
              case Perm => PermLeCmp(l, r)(pos, info)
              case _ => sys.error("unexpected type")
            }
          case PSymOp.Gt =>
            l.typ match {
              case Int => GtCmp(l, r)(pos, info)
              case Perm => PermGtCmp(l, r)(pos, info)
              case _ => sys.error("unexpected type " + l.typ.toString())
            }
          case PSymOp.Ge =>
            l.typ match {
              case Int => GeCmp(l, r)(pos, info)
              case Perm => PermGeCmp(l, r)(pos, info)
              case _ => sys.error("unexpected type")
            }
          case PSymOp.EqEq => EqCmp(l, r)(pos, info)
          case PSymOp.Ne => NeCmp(l, r)(pos, info)
          case PSymOp.Implies => Implies(l, r)(pos, info)
          case PSymOp.Wand => MagicWand(l, r)(pos, info)
          case PSymOp.Iff => EqCmp(l, r)(pos, info)
          case PSymOp.AndAnd => And(l, r)(pos, info)
          case PSymOp.OrOr => Or(l, r)(pos, info)

          case PKwOp.In => right.typ match {
            case _: PSeqType => SeqContains(l, r)(pos, info)
            case _: PMapType => MapContains(l, r)(pos, info)
            case _: PSetType | _: PMultisetType => AnySetContains(l, r)(pos, info)
            case t => sys.error(s"unexpected type $t")
          }

          case PSymOp.Append => SeqAppend(l, r)(pos, info)
          case PKwOp.Subset => AnySetSubset(l, r)(pos, info)
          case PKwOp.Intersection => AnySetIntersection(l, r)(pos, info)
          case PKwOp.Union => AnySetUnion(l, r)(pos, info)
          case PKwOp.Setminus => AnySetMinus(l, r)(pos, info)
          case _ => sys.error(s"unexpected operator $op")
        }
      case PUnExp(op, pe) =>
        val e = exp(pe)
        op.rs match {
          case PSymOp.Minus =>
            e.typ match {
              case Int => Minus(e)(pos, info)
              case Perm => PermMinus(e)(pos, info)
              case _ => sys.error("unexpected type")
            }
          case PSymOp.Not => Not(e)(pos, info)
        }
      case PInhaleExhaleExp(_, in, _, ex, _) =>
        InhaleExhaleExp(exp(in), exp(ex))(pos, info)
      case PIntLit(i) =>
        IntLit(i)(pos, info)
      case p@PResultLit(_) =>
        // find function
        var par: PNode = p.parent.get
        while (!par.isInstanceOf[PFunction]) {
          if (par == null) sys.error("cannot use 'result' outside of function")
          par = par.parent.get
        }
        Result(ttyp(par.asInstanceOf[PFunction].typ.resultType))(pos, info)
      case PBoolLit(_, b) =>
        if (b) TrueLit()(pos, info) else FalseLit()(pos, info)
      case PNullLit(_) =>
        NullLit()(pos, info)
      case PFieldAccess(rcv, _, idn) =>
        FieldAccess(exp(rcv), findField(idn))(pos, info)
      case PMagicWandExp(left, _, right) => MagicWand(exp(left), exp(right))(pos, info)
      case pfa@PCall(func, _, args, _, _) =>
        members(func.name) match {
          case f: Function => FuncApp(f, args map exp)(pos, info)
          case f @ DomainFunc(_, _, _, _, _) =>
            val actualArgs = args map exp
            /* TODO: Not used - problem?*/
            type TypeSubstitution = Map[TypeVar, Type]
            val so : Option[TypeSubstitution] = pfa.domainSubstitution match{
              case Some(ps) => Some(ps.m.map(kv=>TypeVar(kv._1)->ttyp(kv._2)))
              case None => None
            }
            so match {
              case Some(s) =>
                val d = members(f.domainName).asInstanceOf[Domain]
                assert(s.keys.toSet.subsetOf(d.typVars.toSet))
                val sp = s //completeWithDefault(d.typVars,s)
                assert(sp.keys.toSet == d.typVars.toSet)
                if (f.interpretation.isDefined)
                  BackendFuncApp(f, actualArgs)(pos, info)
                else
                  DomainFuncApp(f, actualArgs, sp)(pos, info)
              case _ => sys.error("type unification error - should report and not crash")
            }
          case _: Predicate =>
            val inner = PredicateAccess(args map exp, findPredicate(func).name) (pos, info)
            val fullPerm = FullPerm()(pos, info)
            PredicateAccessPredicate(inner, fullPerm) (pos, info)
          case _ => sys.error("unexpected reference to non-function")
        }
      case PNewExp(_, _) => sys.error("unexpected `new` expression")
      case PUnfolding(_, loc, _, e) =>
        Unfolding(exp(loc).asInstanceOf[PredicateAccessPredicate], exp(e))(pos, info)
      case PApplying(_, wand, _, e) =>
        Applying(exp(wand).asInstanceOf[MagicWand], exp(e))(pos, info)
      case PLet(_, variable, _, exp1, _, PLetNestedScope(body)) =>
        Let(liftLogicalDecl(variable), exp(exp1), exp(body))(pos, info)
      case _: PLetNestedScope =>
        sys.error("unexpected node PLetNestedScope, should only occur as a direct child of PLet nodes")
      case PExists(_, vars, _, triggers, e) =>
        val ts = triggers map (t => Trigger((t.exp map exp) map (e => e match {
          case PredicateAccessPredicate(inner, _) => inner
          case _ => e
        }))(t))
        Exists(vars map liftLogicalDecl, ts, exp(e))(pos, info)
      case PForall(_, vars, _, triggers, e) =>
        val ts = triggers map (t => Trigger((t.exp map exp) map (e => e match {
          case PredicateAccessPredicate(inner, _) => inner
          case _ => e
        }))(t))
        val fa = Forall(vars map liftLogicalDecl, ts, exp(e))(pos, info)
        if (fa.isPure) {
          fa
        } else {
          val desugaredForalls = QuantifiedPermissions.desugarSourceQuantifiedPermissionSyntax(fa)
          desugaredForalls.tail.foldLeft(desugaredForalls.head: Exp)((conjuncts, forall) =>
            And(conjuncts, forall)(fa.pos, fa.info, fa.errT))
        }
      case PForPerm(_, vars, res, _, e) =>
        val varList = vars map liftLogicalDecl
        exp(res) match {
          case PredicateAccessPredicate(inner, _) => ForPerm(varList, inner, exp(e))(pos, info)
          case f : FieldAccess => ForPerm(varList, f, exp(e))(pos, info)
          case p : PredicateAccess => ForPerm(varList, p, exp(e))(pos, info)
          case w : MagicWand => ForPerm(varList, w, exp(e))(pos, info)
          case other =>
            sys.error(s"Internal Error: Unexpectedly found $other in forperm")
        }
      case POldExp(_, lbl, _, e, _) =>
        lbl.map(l => LabelledOld(exp(e), l.name)(pos, info)).getOrElse(Old(exp(e))(pos, info))
      case PCondExp(cond, _, thn, _, els) =>
        CondExp(exp(cond), exp(thn), exp(els))(pos, info)
      case PCurPerm(_, _, res, _) =>
        exp(res) match {
          case PredicateAccessPredicate(inner, _) => CurrentPerm(inner)(pos, info)
          case x: FieldAccess => CurrentPerm(x)(pos, info)
          case x: PredicateAccess => CurrentPerm(x)(pos, info)
          case x: MagicWand => CurrentPerm(x)(pos, info)
          case other => sys.error(s"Unexpectedly found $other")
        }
      case PNoPerm(_) =>
        NoPerm()(pos, info)
      case PFullPerm(_) =>
        FullPerm()(pos, info)
      case PWildcard(_) =>
        WildcardPerm()(pos, info)
      case PEpsilon(_) =>
        EpsilonPerm()(pos, info)
      case PAccPred(_, loc, perm) =>
        val p = exp(perm)
        exp(loc) match {
          case loc@FieldAccess(_, _) =>
            FieldAccessPredicate(loc, p)(pos, info)
          case loc@PredicateAccess(_, _) =>
            PredicateAccessPredicate(loc, p)(pos, info)
          case PredicateAccessPredicate(inner, _) => PredicateAccessPredicate(inner, p)(pos, info)
          case _ =>
            sys.error("unexpected location")
        }
      case _: PEmptySeq =>
        EmptySeq(ttyp(pexp.typ.asInstanceOf[PSeqType].elementType))(pos, info)
      case PExplicitSeq(_, _, elems, _) =>
        ExplicitSeq(elems map exp)(pos, info)
      case PRangeSeq(_, low, _, high, _) =>
        RangeSeq(exp(low), exp(high))(pos, info)

      case PLookup(base, _, index, _) => base.typ match {
        case _: PSeqType => SeqIndex(exp(base), exp(index))(pos, info)
        case _: PMapType => MapLookup(exp(base), exp(index))(pos, info)
        case t => sys.error(s"unexpected type $t")
      }

      case PSeqSlice(seq, _, s, _, e, _) =>
        val es = exp(seq)
        val ss = s.map(exp).map(SeqTake(es, _)(pos, info)).getOrElse(es)
        e.map(exp).map(SeqDrop(ss, _)(pos, info)).getOrElse(ss)

      case PUpdate(base, _, key, _, value, _) => base.typ match {
        case _: PSeqType => SeqUpdate(exp(base), exp(key), exp(value))(pos, info)
        case _: PMapType => MapUpdate(exp(base), exp(key), exp(value))(pos, info)
        case t => sys.error(s"unexpected type $t")
      }

      case PSize(base) => base.typ match {
        case _: PSeqType => SeqLength(exp(base))(pos, info)
        case _: PMapType => MapCardinality(exp(base))(pos, info)
        case _: PSetType | _: PMultisetType => AnySetCardinality(exp(base))(pos, info)
        case t => sys.error(s"unexpected type $t")
      }

      case _: PEmptySet =>
        EmptySet(ttyp(pexp.typ.asInstanceOf[PSetType].elementType))(pos, info)
      case PExplicitSet(_, _, elems, _) =>
        ExplicitSet(elems map exp)(pos, info)
      case _: PEmptyMultiset =>
        EmptyMultiset(ttyp(pexp.typ.asInstanceOf[PMultisetType].elementType))(pos, info)
      case PExplicitMultiset(_, _, elems, _) =>
        ExplicitMultiset(elems map exp)(pos, info)

      case _: PEmptyMap => EmptyMap(
        ttyp(pexp.typ.asInstanceOf[PMapType].keyType),
        ttyp(pexp.typ.asInstanceOf[PMapType].valueType)
      )(pos, info)
      case PExplicitMap(_, _, elems, _) =>
        ExplicitMap(elems map exp)(pos, info)
      case PMaplet(key, value) =>
        Maplet(exp(key), exp(value))(pos, info)
      case PMapDomain(_, base) =>
        MapDomain(exp(base))(pos, info)
      case PMapRange(_, base) =>
        MapRange(exp(base))(pos, info)

      case t: PExtender => t.translateExp(this)
    }
  }

  implicit def liftPos(node: Where): SourcePosition = Translator.liftWhere(node)

  /** Takes a `PAnyFormalArgDecl` and turns it into a `AnyLocalVarDecl`. */
  def liftAnyArgDecl(formal: PAnyFormalArgDecl) =
    formal match {
      case f: PFormalArgDecl => liftArgDecl(f)
      case u: PUnnamedFormalArgDecl => UnnamedLocalVarDecl(ttyp(u.typ))(u.typ)
    }

  /** Takes a `PFormalArgDecl` and turns it into a `LocalVarDecl`. */
  def liftArgDecl(formal: PFormalArgDecl) =
      LocalVarDecl(formal.idndef.name, ttyp(formal.typ))(formal.idndef)

  /** Takes a `PFormalReturnDecl` and turns it into a `LocalVarDecl`. */
  def liftReturnDecl(formal: PFormalReturnDecl) =
      LocalVarDecl(formal.idndef.name, ttyp(formal.typ))(formal.idndef)

  /** Takes a `PLogicalVarDecl` and turns it into a `LocalVarDecl`. */
  def liftLogicalDecl(logical: PLogicalVarDecl) =
      LocalVarDecl(logical.idndef.name, ttyp(logical.typ))(logical.idndef)

  /** Takes a `PType` and turns it into a `Type`. */
  def ttyp(t: PType): Type = t match {
    case PPrimitiv(name) => name.rs match {
      case PKw.Int => Int
      case PKw.Bool => Bool
      case PKw.Ref => Ref
      case PKw.Perm => Perm
    }
    case PSeqType(_, elemType) =>
      SeqType(ttyp(elemType))
    case PSetType(_, elemType) =>
      SetType(ttyp(elemType))
    case PMultisetType(_, elemType) =>
      MultisetType(ttyp(elemType))
    case PMapType(_, keyType, valueType) =>
      MapType(ttyp(keyType), ttyp(valueType))
    case PDomainType(name, args) =>
      members.get(name.name) match {
        case Some(domain: Domain) =>
          if (domain.interpretations.isDefined) {
            BackendType(domain.name, domain.interpretations.get)
          } else {
            val typVarMapping = domain.typVars zip (args map ttyp)
            DomainType(domain, typVarMapping /*.filter {
            case (tv, tt) => tv!=tt //!tt.isInstanceOf[TypeVar]
          }*/.toMap)
          }
        case Some(adt: Adt) =>
          val typVarMapping = adt.typVars zip (args map ttyp)
          AdtType(adt, typVarMapping.toMap)
        case Some(other) =>
          sys.error(s"Did not expect member ${other}")
        case None =>
          assert(args.isEmpty)
          TypeVar(name.name) // not a domain, i.e. it must be a type variable
      }
    case PWandType() => Wand
    case t: PExtender => t.translateType(this)
    case PUnknown() =>
      sys.error("unknown type unexpected here")
    case _: PFunctionType =>
      sys.error("unexpected use of internal typ")
  }
}

object Translator {
  /** Takes a [[viper.silver.parser.FastPositioned]] and turns it into a [[viper.silver.ast.SourcePosition]]. */
  implicit def liftWhere(node: Where): SourcePosition = {
    if (node.pos._1.isInstanceOf[FilePosition]) {
      assert(node.pos._2.isInstanceOf[FilePosition])

      val begin = node.pos._1.asInstanceOf[FilePosition]
      val end = node.pos._2.asInstanceOf[FilePosition]

      SourcePosition(begin.file,
        LineColumnPosition(begin.line, begin.column),
        LineColumnPosition(end.line, end.column))
    }
    else {
      SourcePosition(null, 0, 0)
    }
  }

  def toInfo(annotations: Seq[PAnnotation], node: PNode): Info = {
    if (annotations.isEmpty) {
      NoInfo
    } else {
      AnnotationInfo(annotations.groupBy(_.key).map{ case (k, v) => k.name -> v.flatMap(_.values.map(_.s)) })
    }
  }
}
