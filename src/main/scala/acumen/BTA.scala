/**
 * Binding time analysis
 * Authors: Yingfu Zeng, Walid Taha
 */

package acumen
import Pretty._
import Simplifier._
import GE._
import annotation.tailrec

/** Performs automatic binding time analysis and offline specialization.
 *  
 *  BTA is done by first generating dependence constraints from an input AST then
 *  solving them to produce the annotated AST with static/dynamic binding time in every node.
 *  
 *  Specialization takes the annotated AST and performs partial evaluation to produce
 *  a specialized AST (residual program).
 * 
 * 
 * */
object BindingTimeAnalysis {
  import Specialization._
  type Label = Int
  type ScopeEnv = scala.collection.immutable.Map[Var, Label]
  type LabelEnv = scala.collection.immutable.Map[Label, ScopeEnv]
  def emptyScopeEnv = scala.collection.immutable.Map[Var, Label]()
  def emptyLabelEnv = scala.collection.immutable.Map[Label, ScopeEnv]()
  def initLabelEnv = emptyLabelEnv + ((root, emptyScopeEnv))
  /* The starting point of label */
  def root = -1

  def run(t: Prog) = {
    t match {
      case Prog(defs) =>
        Prog(defs.map(d => d match {
          case ClassDef(n, f, d, b) => n.x match{
            case "Device" => ClassDef(n, f, d, b)
            case _ => bta(ClassDef(n, f, d, b))
          }
        }))
    }
  }
  /** Partial evaluation of a class
   *  
   *  First generate the annotated AST
   *  Then perform partial evaluation using method specializeListActions 
   *  
   * @param ast the input ast
   * */
  def bta(ast: ClassDef): ClassDef = {
    // Annotate AST
    val aprog = labelAst(ast, 0)
    val labelEnv = aprog._4
    val anActions = aprog._1.body
    val env = aprog._2
    // Generate constraint set
    val constraints = anActions.foldLeft(List[Constraint]())((r, x) => r ::: traversal(x, root, labelEnv))
    val S = initConstraintSet(constraints)
    // Constraint solving
    val (newC, newS) = exhaustNormalize(constraints, S)
    val sortedSubstitution = newS.toList.sortWith((x, y) => x._1 < y._1)
    val btaResult: Map[Label, Constraint] = sortedSubstitution.map(x => (x._1, x._2.bt match {
      case Stat => Known(x._1)
      case Dyn  => Unknown(x._1)
    })).toMap
    // Apply specialization using BTA result
    val newAst = ast match {
      case ClassDef(n, f, p, _) =>
        val temp = specializeListActions(anActions, btaResult, Map[Expr, Expr](), List[Var](), Nil)
        val (actions, neweqs) = (temp._1, temp._3.distinct)
        val newInits = neweqs.flatMap(x => x match {
          case Continuously(a @ Equation(lhs, rhs)) => (lhs,rhs) match{
            case (Var(n), ExprVector(l)) => 
              (for (p <- 0 to n.primes) yield
                  Init(Name(n.x, p), ExprRhs(ExprVector(l.map(x => Lit(GInt(0)))))))
            case (Var(n), _) => (for (p <- 0 to n.primes) yield Init(Name(n.x, p),ExprRhs(Lit(GInt(0)))))
          }
           
        })
        ClassDef(n, f, p ::: newInits, actions)
    }
    // Clean the hash table in symbolic differentiation after each class
    SD.clear()
    // Return specialized AST
    newAst
  }

  def labelListExpr(es: List[Expr], label: Label)(implicit fields: List[Name]): (List[AExpr[Label]], Label) = {
    val aes_label = (List[AExpr[Label]](), label)
    val result = es.foldLeft(aes_label)((r, x) => {
      val exresult = labelAst(x, r._2)
      (exresult._1 :: r._1, exresult._2)
    })
    (result._1.reverse, result._2)
  }
  /* Annotate expression with a label, return the annotated ast and the next available label */
  def labelAst(e: Expr, label: Label)(implicit fields: List[Name]): (AExpr[Label], Label) = e match {
    case Lit(gv)   => (ALit(gv, label), label + 1)
    case Var(name) => (AVar(name, label), label + 1)
    case TypeOf(cn) => (ATypeOf(cn,label),label + 1)
    case Index(e, idx) =>
      val ae = labelAst(e, label)
      val aidx = labelListExpr(idx, ae._2)
      (AIndex(ae._1, aidx._1, aidx._2), aidx._2 + 1)
    case Sum(e, i, col, cond) =>
      val ae = labelAst(e, label + 1)
      val acol = labelAst(col, ae._2)
      val acond = labelAst(cond, acol._2)
      (ASum(ae._1, AVar(i, label), acol._1, acond._1, acond._2), acond._2 + 1)
    case Op(f, es) =>
      if (fields contains f) {
        labelAst(Index(Var(f), es), label)
      } else {
        val aresult = labelListExpr(es, label)
        (AOp(f, aresult._1, aresult._2), aresult._2 + 1)
      }
    case ExprVector(es) => {
      val aresult = labelListExpr(es, label)
      (AExprVector(aresult._1, aresult._2), aresult._2 + 1)
    }
    case Dot(expr, name) => {
      val aexpr = labelAst(expr, label)
      (ADot(aexpr._1, name, aexpr._2), aexpr._2 + 1)
    }
    case Quest(expr, name) => {
      val aexpr = labelAst(expr, label)
      (AQuest(aexpr._1, name, aexpr._2), aexpr._2 + 1)
    }
    case ExprLet(bindings, expr) => {
      val (abindings,nextLabel,newFields) = bindings.foldLeft((List[(AVar[Label], AExpr[Label])](),label,fields)){
        case (r,binding) => 
          val avar = AVar(binding._1, r._2)
          val arhs = labelAst(binding._2, r._2 + 1)(r._3)
          ((avar, arhs._1) :: r._1,arhs._2, avar.name ::r._3)      
      }  
      val aexpr = labelAst(expr, nextLabel)(newFields)
      (AExprLet(abindings.reverse, aexpr._1, aexpr._2), aexpr._2 + 1)
    }
    case ExprInterval(e1, e2) =>
      val (ae1, ae2) = (labelAst(e1, label), labelAst(e2, label))
      (AExprInterval(ae1._1, ae2._1, label), label)
    case Call(f, es) =>
      val af = labelAst(f, label)
      val aresult = labelListExpr(es, af._2)
      (ACall(af._1, aresult._1, aresult._2), aresult._2 + 1)
  }
  
  def labelAst(a: Action, label: Label, scope: Label, env: LabelEnv)(implicit fields: List[Name]): (AAction[Label], Label, LabelEnv) = a match {
    case Continuously(a) => a match {
      case Equation(lhs, rhs) => lhs match {
        case v: Var =>
          val alhs = labelAst(lhs, label)
          val arhs = labelAst(rhs, alhs._2)
          val newScopeEnv = env(scope) + ((v, label))
          val newEnv = env.updated(scope, newScopeEnv)
          (AContinuously(AEquation(alhs._1, arhs._1, arhs._2), arhs._2), arhs._2 + 1, newEnv)
        case _ =>
          val alhs = labelAst(lhs, label)
          val arhs = labelAst(rhs, alhs._2)
          (AContinuously(AEquation(alhs._1, arhs._1, arhs._2), arhs._2), arhs._2 + 1, env)
      }
      case Assignment(lhs, rhs) =>
        val alhs = labelAst(lhs, label)
        val arhs = labelAst(rhs, alhs._2)
        (AContinuously(AEquation(alhs._1, arhs._1, arhs._2), arhs._2), arhs._2 + 1, env)
      case EquationT(lhs, rhs) =>
        val alhs = labelAst(lhs, label)
        val arhs = labelAst(rhs, alhs._2)
        (AContinuously(AEquation(alhs._1, arhs._1, arhs._2), arhs._2), arhs._2 + 1, env)
    }
    case Discretely(a) => a match {
      case Assign(lhs, rhs) =>
        val alhs = labelAst(lhs, label)
        val arhs = labelAst(rhs, alhs._2)
        (ADiscretely(AAssign(alhs._1, arhs._1, arhs._2), arhs._2), arhs._2 + 1, env)
      case Create(x, name, args) =>
        val aargs = labelListExpr(args, label)
        (ADiscretely(ACreate(x, name, aargs._1, aargs._2), aargs._2), aargs._2 + 1, env)
      case Elim(e) =>
        val aexpr = labelAst(e, label)
        (ADiscretely(AElim(aexpr._1, aexpr._2), aexpr._2), aexpr._2 + 1, env)
      case Move(obj, newParent) =>
        val aobj = labelAst(obj, label)
        val ap = labelAst(newParent, aobj._2)
        (ADiscretely(AMove(aobj._1, ap._1, ap._2), ap._2), ap._2 + 1, env)
    }
    case IfThenElse(cond, t, e) =>
      val acond = labelAst(cond, label)
      val scope1 = acond._2
      val at = labelListAst(t, acond._2, scope1, env + ((scope1, env(scope))))
      val scope2 = at._2
      val ae = labelListAst(e, at._2, scope2, env + ((scope2, env(scope))))
      (AIfThenElse(acond._1, at._1, ae._1, ae._2, scope1, scope2), ae._2 + 1, env ++ at._3 ++ ae._3)
    case Switch(cond, clauses) =>
      val acond = labelAst(cond, label)
      val aClauses = labelListClauses(clauses, acond._2, scope, env)
      (ASwitch(acond._1, aClauses._1, aClauses._2), aClauses._2 + 1, aClauses._3)
    case ForEach(i, q, es) =>
      val aq = labelAst(q, label + 1)
      val aes = labelListAst(es, aq._2, scope, env)
      (AForEach(i, aq._1, aes._1, aes._2), aes._2 + 1, aes._3)
    case Claim(e) =>
      val aq = labelAst(e, label)
      (AClaim(aq._1, aq._2), aq._2 + 1, env)
    case Hypothesis(s, e) =>
      val aq = labelAst(e, label)
      (AHypothesis(s, aq._1, aq._2), aq._2 + 1, env)

  }

  def labelAst(ast: Prog, label: Label): (AProg[Label], EnvVars) = {
    def labelClasses(cs:List[ClassDef],l:Label):(List[AClassDef[Label]],Label,EnvVars) = {
      cs.foldLeft((List[AClassDef[Label]](),l,new EnvVars() )){case (r,d) =>
           val aClass = labelAst(d,r._2)
           (aClass._1::r._1, aClass._3, r._3 ++ aClass._2)
         }}   
    ast match {
      case Prog(defs) =>
        val lprog = labelClasses(defs, label)
        (AProg(lprog._1), lprog._3)
    }
  }
  def labelAst(ast: ClassDef, label: Label): (AClassDef[Label], EnvVars, Label, LabelEnv) = {
    ast match {
      case ClassDef(cName, fields, privs, body) =>
        {
          val priVars = privs.map(_.x)
          val initResult = labelListAst(privs, label, new EnvVars())
          val (afields,counter,aenv) = 
            fields.foldLeft((List[AName[Label]](),initResult._2,new EnvVars()))((r,field) =>
              (AName(field, r._2) :: r._1, r._2 + 1,  r._3 + (Var(field) -> r._2))
            )
          val actionResult = labelListAst(body, counter, root, initLabelEnv)(fields ::: priVars)
          (AClassDef(cName, afields, initResult._1, actionResult._1), aenv, actionResult._2, actionResult._3)
        }
    }
  }

  def labelListAst(actions: List[Action], initLabel: Label, scope: Label, env: LabelEnv)(implicit fields: List[Name]): (List[AAction[Label]], Label, LabelEnv) = {
    val result = actions.foldLeft((List[AAction[Label]](), initLabel,scope,env))((r, x) => {
      val exresult = labelAst(x, r._2,r._3,r._4)
      (exresult._1 :: r._1, exresult._2,scope,r._4 ++ exresult._3)
    })
    (result._1.reverse, result._2, result._4)
  }
  // Label a list of Inits, return the next available label and
  // the environment with variable -> label 
  def labelListAst(inits: List[Init], initLabel: Label, initEnv: EnvVars): (List[AInit[Label]], Label, EnvVars) = {
    val result = inits.foldLeft( (List[AInit[Label]](), initLabel,new EnvVars()))((r, x) => {
      val ainit = AInit(AVar(x.x, r._2), x.rhs)
      (ainit::r._1, r._2 + 1, r._3 + (Var(x.x) -> r._2) )
    })
    (result._1.reverse, result._2, result._3)
  }

  def labelListClauses(clauses: List[Clause], initLabel: Label, scope: Label, env: LabelEnv)(implicit fields: List[Name]): (List[AClause[Label]], Label, LabelEnv) = {
    val (aClauses, nextLabel, labelEnv) = clauses.foldLeft((List[AClause[Label]](), initLabel, env)) {
      case (r, clause) =>
        val aasertion = labelAst(clause.assertion, r._2)
        val clauseScope = r._2
        val arhs = labelListAst(clause.rhs, aasertion._2, clauseScope, r._3 + ((clauseScope, r._3(scope))))
        val aclause = AClause(clause.lhs, aasertion._1, arhs._1, arhs._2, clauseScope)
        (aclause::r._1, arhs._2 + 1, r._3 ++ arhs._3)
    }
    (aClauses.reverse, nextLabel, labelEnv)
  }

  def getDirectedVars(env: LabelEnv): List[Var] = {
    val dvars = 
     (for ((s, se) <- env)
       yield for ((v, l) <- se)  yield v).flatten.toList
    dvars.foldLeft(List[Var]())((r, x) => x match {
      case Var(Name(n, p)) =>
        if (p > 0)
          (for (i <- 0 to p) yield Var(Name(n, i))).toList ::: r
        else
          x :: r
    }).toSet.toList
  }
  // Map with bindings of variable to its label, which stay constant during BTA phase
  type EnvVars = scala.collection.immutable.HashMap[Var, Label]
  type LabelKnowledge = scala.collection.immutable.HashMap[Int, Boolean]
  val emptyCs: List[Constraint] = List.empty

  // Traversal ast and generate constraints
  def traversal(e: AExpr[Label], env: ScopeEnv): List[Constraint] = e match {
    case ALit(gv, l) => List(Known(l))
    case AVar(name, l) =>
      def lookup(v: Var, l: Label) = {
        if (env.contains(v))
          NLT(env(v), l)
        else
          Unknown(l)
      }
      if (name.x != "pi") {
        lookup(Var(name), l) ::
          (for (i <- 0 to name.primes - 1) yield if (env.contains(Var(Name(name.x, i))))
            Unknown(env(Var(Name(name.x, i))))
          else
            NLT(l, l)).toList
      } else
        Nil
    case AIndex(e, idx, l) =>
      NLT(e.an, l) ::
        traversal(e, env) :::
        idx.foldLeft(List[Constraint]())((r, x) => traversal(x, env) ::: r) :::
        idx.map(x => NLT(x.an, l))
    case AOp(f, es, l) => f.x match {
      case "length" =>
        Known(l) :: es.foldLeft(List[Constraint]())((r, x) => traversal(x, env) ::: r)
      case _ =>
        es.foldLeft(List[Constraint]())((r, x) => traversal(x, env) ::: r) :::
          es.map(x => NLT(x.an, l))
    }
    case ADot(aexpr, n, l) => List(Unknown(l)) ::: traversal(aexpr, env)
    case ASum(ae, i, acol, acond, l) =>
      NLT(ae.an, l) :: NLT(acond.an, l) :: NLT(acol.an, l) :: NLT(acol.an, i.an) ::
        traversal(acond, (env + (i.expr -> i.an))) :::
        traversal(acol, env) :::
        traversal(ae, (env + (i.expr -> i.an)))

    case AExprVector(es, l) =>
      es.map(x => NLT(x.an, l)) :::
        es.foldLeft(List[Constraint]())((r, x) => traversal(x, env) ::: r)

    case AExprLet(bindings, expr, l) => {
      val newEnv = bindings.foldLeft(env)((r, x) => r + (Var(x._1.name) -> x._1.an))
      bindings.foldLeft(List[Constraint]())((r, x) => traversal(x._2, newEnv) ::: r) :::
        bindings.map(x => NLT(x._2.an, x._1.an)) :::
        List(NLT(expr.an, l)) :::
        traversal(expr, newEnv)
    }
    case AExprInterval(ae1, ae2, l) => Nil
    case ACall(af, aes, l) =>
      NLT(af.an, l) ::
        aes.map(x => NLT(x.an, l)) :::
        aes.foldLeft(List[Constraint]())((r, x) => traversal(x, env) ::: r)
    case _ => Nil
  }

  // Return annotated action and the next available label
  def traversal(a: AAction[Label], scope: Label, env: LabelEnv): List[Constraint] = {
    def genNLTS(lhs: Expr, rhs: Expr, l: Label): List[NLTS] = {
      def dropIndex[T](xs: List[T], n: Int): List[T] = {
        val (l1, l2) = xs splitAt n
        l1 ::: (l2 drop 1)
      }
      val vars = findVars(lhs, Map[Expr, Expr]())(List.empty) ::: findVars(rhs, Map[Expr, Expr]())(List.empty)
      (for (i <- 0 to vars.length - 1) yield NLTS(l, dropIndex(vars, i).map(env(scope)(_)))).toList
    }
    a match {
      case AContinuously(c, _) => c match {
        case AEquation(lhs, rhs, l) => lhs match {
          case AVar(name, _) => name.primes match {
            case 0 =>
              NLT(rhs.an, env(scope)(Var(name))) ::
                NLT(rhs.an, l) ::
                traversal(rhs, env(scope))
            case n =>
              NLT(rhs.an, env(scope)(Var(name))) ::
                NLT(rhs.an, l) ::
                (for (i <- 0 to name.primes - 1) yield if (env(scope).contains(Var(Name(name.x, i))))
                  Unknown(env(scope)(Var(Name(name.x, i))))
                else
                  NLT(l, l)).toList :::
                traversal(rhs, env(scope))
          }
          case _ =>
            NLT(lhs.an, l) :: NLT(rhs.an, l) ::
              (traversal(lhs, env(scope)) :::
                traversal(rhs, env(scope)))
        }
      }
      case ADiscretely(c, l1) => c match {
        case AAssign(lhs, rhs, l) => lhs match {
          // Discrete assignment will be regarded as unknown and leave as it is 
          case AVar(name, _) =>
            Unknown(l) :: Unknown(l1) :: traversal(rhs, env(scope))
          case ADot(_, _, _) => Unknown(l) :: Unknown(l1) :: traversal(rhs, env(scope))
          case _ =>
            NLT(lhs.an, l) :: NLT(rhs.an, l) ::
              (traversal(lhs, env(scope)) :::
                traversal(rhs, env(scope)))
        }
        case ACreate(_, _, args, l) =>
          Unknown(l) :: args.foldLeft(List[Constraint]())((r, x) => traversal(x, env(scope)) ::: r)
        case AElim(_, l)    => Unknown(l) :: Nil
        case AMove(_, _, l) => Unknown(l) :: Nil
      }
      case AClaim(aq, l) =>
        traversal(aq, env(scope))
      case AHypothesis(_, aq, l) =>
        traversal(aq, env(scope))

      case AIfThenElse(cond, t, e, l, scope1, scope2) =>
        NLT(cond.an, l) ::
          traversal(cond, env(scope)) :::
          t.foldLeft(List[Constraint]())((r, x) => traversal(x, scope1, env) ::: r) :::
          e.foldLeft(List[Constraint]())((r, x) => traversal(x, scope2, env) ::: r)

      case AForEach(i, q, aes, l) =>
        NLT(q.an, l) :: traversal(q, env(scope)) :::
          aes.map(x => NLT(x.an, l)) :::
          aes.foldLeft(List[Constraint]())((r, x) => traversal(x, scope, env) ::: r)

      case ASwitch(cond, clauses, l) =>
        NLT(cond.an, l) ::
          traversal(cond, env(scope)) :::
          clauses.map(x => x.rhs.map(y => NLT(cond.an, y.an))).flatten :::
          clauses.foldLeft(List[Constraint]())((r, x) =>
            x.rhs.foldLeft(List[Constraint]())((r1, x1) =>
              traversal(x.assertion, env(x.scope)) ::: traversal(x1, x.scope, env) ::: r1) ::: r)
    }
  }

  // Constraint normalization
  abstract class BT // Binding time
  case object Stat extends BT // Static
  case object Dyn extends BT // Dynamic
  case class BVar(label: Label) extends BT { // Binding time variable
    override def toString = "b" + label.toString
  }
  case class Node(val label: Label, val bt: BT, 
     // List of other nodes that depends on this node
     val deps : Set[Node],
     // List of nodes this node depends on
     val depons: Set[Node]) 
     {
     /* Factory methods */
     def updateBT(newBt:BT) = Node(label,newBt,deps,depons)
     def updateDeps(newDeps:Set[Node]) = Node(label,bt,newDeps,depons)
     def updateDepons(newDepons:Set[Node]) = Node(label,bt,deps,newDepons)
     override def toString = bt.toString() + " " +
      deps.map(node => node.label).toString
  }
  type Substitution = scala.collection.immutable.Map[Label, Node]

  // Modify node in S, or initialize it
  def link(label: Label, bt: BT, S: Substitution): Substitution = {
    if (S.contains(label)) {
      val node = S(label)
      bt match {
        case BVar(l2) => S.updated(label, node.updateBT(bt))
        // Modify its bt to either Stat or Dyn
        // And check weather conflict happen
        case bt => S(label).bt match {
          case Stat | Dyn =>
            S.updated(label, node.updateBT(bt))
            // Update label2 -> label to label2 -> bt in S 
            S.find(x => x._2.bt == BVar(label)) match {
              case Some((l, node)) => link(l, bt, S)
              case None            =>
            }
          case BVar(_) =>  S.updated(label, node.updateBT(bt))
        }
      }
    S
    } 
    else {
       S + ((label, new Node(label,bt, Set[Node](), Set[Node]())))
    }
  }

  // Generate a node for every label, and its corresponding dependences, stored in Substitution
  def initConstraintSet(C: List[Constraint]): Substitution = {
    val S = scala.collection.immutable.Map[Label, Node]()
    C.foldLeft(S)((r,x) => x match{
        case Known(l)   =>  link(l, Stat, r)
        case Unknown(l) =>  link(l, Dyn, r)
        case NLT(l1, l2) => {
         val r1 =  link(l1, BVar(l1), r)
         val r2 =  link(l2, BVar(l2), r1)
         val newNode1 = r2(l1).updateDeps(r2(l1).deps + r2(l2))
         val newNode2 = r2(l2).updateDepons(r2(l2).depons + r2(l1))
         r2.updated(l1,newNode1).updated(l2,newNode2)
        }
        case NLTS(_, _) => r
        case Equals(l1, l2) =>
          val r1 = link(l1, BVar(l1), r)
          val r2 = link(l2, BVar(l2), r1)
          r2
    })
    
  }

  // Normalize constraint set until reach fixed point
  def exhaustNormalize(C: List[Constraint], S: Substitution): (List[Constraint], Substitution) = {
     @tailrec def  normalizeTillFixedpoint (C: List[Constraint], S: Substitution):(List[Constraint], Substitution) = {
      val (dnewC, dnewS) = normalize(C, S)
      if (dnewC.size < C.size)
        normalizeTillFixedpoint(dnewC,dnewS)
      else
        (dnewC,dnewS)
    }
    val (normalC, normalS) = normalizeTillFixedpoint(C,S)

    // Sort the remaining constraints
    val (prior,rest) = normalC.foldLeft((List[Constraint](),List[Constraint]())){(r,c) => 
      c match {
      case NLT(l1, l2) => (normalS(l1).bt, normalS(l2).bt) match {
        case (BVar(_), Dyn) | (Dyn, BVar(_)) | (BVar(_), BVar(_)) =>(c::r._1,r._2) 
        case _ =>  (r._1,c::r._2)
      }
      case _ => r
    }}
   
    val newC = prior ::: rest
    val (finalS, finalC) = newC.foldLeft((normalS,newC)){case ((rS,rC),c) => c match {
      case NLT(l1, l2) => (rS(l1).bt, rS(l2).bt) match {
        case (BVar(b1), Dyn) =>
          (union(BVar(b1), Stat, rS),  remove(rC, c))
        case (BVar(b1), BVar(b2)) =>
          (union(BVar(b2), Stat, union(BVar(b1), Stat, rS)), remove(rC, c))
        case (Dyn, BVar(b1)) =>
          (union(BVar(b1), Stat, rS),  remove(rC, c))
        case (Dyn, Dyn) | (Stat, Stat) | (Stat, Dyn) => (rS,remove(rC, c))
        case (Dyn, Stat)                             => sys.error(l1 + "is dyn " + l2 + "is Stat")
        case (Stat, BVar(b)) =>
          (union(BVar(b), Stat, rS),remove(rC, c))
        case (BVar(b), Stat) =>
          (union(BVar(b), Stat, rS), remove(rC, c))
      }
      case NLTS(_, _) => (rS, remove(rC, c))
    }
    }
    
    // Solving all l1->l2 in substitution
    val noBVarS = finalS.foldLeft(finalS)((r,x) => x match{
      case (l,b) => b.bt match{case BVar(l2) => r + ((l,Node(l,Dyn,Set[Node](),Set[Node]())));case _ => r }
    }
      )  
    (finalC, noBVarS)

  }
  def find(l: Label, S: Substitution): BT = S(l).bt
  def union(b1: BT, b2: BT, S: Substitution): Substitution = {
    def unionListNode(bt:BT,nodes:Set[Node],S:Substitution):Substitution={
      nodes.foldLeft(S)((r,x) => union(bt,x.bt,r))
    }
    (b1, b2) match {
      case (Dyn, BVar(l1)) => 
        unionListNode(Dyn,S(l1).deps,link(l1, Dyn, S))    
      case (BVar(l1), Dyn) =>
        unionListNode(Dyn,S(l1).deps,link(l1, Dyn, S))        
      case (BVar(l1), Stat) =>
        link(l1, Stat, S)
      case (Stat, BVar(l1)) =>
        link(l1, Stat, S)
      case (BVar(l1), BVar(l2)) =>
        val newS = link(l1, BVar(l2), S)
        val newNode = newS(l2).updateDeps(newS(l2).deps ++ newS(l1).deps)
        newS.updated(l2,newNode)
      case (Stat, Stat) | (Dyn, Dyn) | (Stat, Dyn) => S
    }
  
  }
  def normalize(C: List[Constraint], S: Substitution): (List[Constraint], Substitution) = {
    def normalizeOne(c:Constraint,S:Substitution):(List[Constraint], Substitution) = c match{
      case Known(l) => (Nil, union(BVar(l), Stat, S))
      case Unknown(l) => (Nil, union(Dyn, BVar(l), S))
      case Equals(l1, l2) => (NLT(l2, l2) :: Nil, union(find(l1, S), find(l2, S), S))
      case NLT(l1, l2) => (find(l1, S), find(l2, S)) match {
        case (Stat, Stat) | (Stat, Dyn) | (Dyn, Dyn) => (Nil, S)
        case (BVar(b), Stat) => (Nil, union(BVar(b), Stat, S))
        case (Dyn, BVar(b)) => (Nil, union(Dyn, BVar(b), S))
        case (Dyn, Stat) =>
          error("Dyn < Stat Error")
        case (Stat, BVar(_)) | (BVar(_), Dyn) | (BVar(_), BVar(_)) => (c::Nil,S)
      }
      case NLTS(l, ls) => (find(l, S), ls.map(find(_, S))) match {
        case (Stat, _) | (Dyn, _) => (Nil, S)
        case (BVar(b), bs) =>
          if (bs.forall(_.isInstanceOf[Stat.type])) {
            (Nil,union(BVar(b), Stat, S))
          }
          else if (bs.exists(_.isInstanceOf[Dyn.type])) {
            (Nil,union(Dyn, BVar(b), S))
          }
          else
           (C,S)
      }
    }
    /* Normalize list of constraint and return new list and substitution */
    C.foldLeft((List[Constraint](),S)){case (r,x) =>
      val one = normalizeOne(x,r._2)
      (r._1:::one._1, one._2)
      }   
  }

  // Unsolved NLT constraint to unknown
  def mkUnknown(cs: List[Constraint]): List[Constraint] = {
    cs.map(x => x match {
      case NLT(l1, l2) => Unknown(l1) :: Unknown(l2) :: List[Constraint]()
      case _           => sys.error("Fail to solve constraints")
    }).flatten.distinct
  }

  def remove(C: List[Constraint], c: Constraint): List[Constraint] = C match {
    case Nil       => Nil
    case d :: Nil  => if (d == c) Nil else d :: Nil
    case d :: tail => if (d == c) remove(tail, c) else d :: remove(tail, c)
  }

  // Pick the equations from input actions, for which GE will transform them into explicit ODE form
  def gaussianElimination(actions: List[Action], directedVars: List[Var], hashMap: Map[Expr, Expr]): List[Action] = {
    val initDAEs = (List[Action](), List[Var](), List[Assign]())
    val (equations, disDirected, disDAEs) = actions.foldLeft(initDAEs)((r, a) => a match {
      case Continuously(e) => (a :: r._1, r._2, r._3)
      case Discretely(Assign(l, right)) =>
        l match {
          case Var(n) => (r._1, Var(n) :: r._2, Assign(l, right) :: r._3)
          case _      => (r._1, r._2, Assign(l, right) :: r._3)
        }
      case _ => r
    })
    
    // Solve discrete algebraic equations as normal equations
    val disODEs = disDAEs match{
      case Nil => Nil
      case _ => 
        val discons = disDAEs.map(x => Continuously(Equation(x.lhs, x.rhs)))
      // When solving discrete algebraic equations, lower derivatives can be regarded as variables
      val nonvariables = directedVars.filterNot(x =>
        directedVars.exists(y =>
          y.name.x == x.name.x &&
            y.name.primes > x.name.primes))
      val disDirectedFiltered = disDirected.map(x => x.name.primes match {
        case 0 => List(x)
        case n => (for (i <- 0 to n) yield Var(Name(x.name.x, i))).toList
      }).flatten
       gaussianElimination(discons, nonvariables ::: disDirectedFiltered, hashMap).map(x =>
        x.asInstanceOf[Continuously].a match {
          case Equation(lhs, rhs) => Discretely(Assign(lhs, rhs))
        })
    }
    val remainActions = actions.diff(equations ::: disDAEs.map(x => Discretely(x)))
    val initTuple = (List[Equation](), List[Action]())
    val (implicitOdes, explicitOdes) = equations.foldLeft(initTuple)((r,x) =>x match {
        case Continuously(Equation(lhs, rhs)) => lhs match {
          case Var(n) =>
            if (!hashMap.contains(Var(n)))
              (r._1,  x :: r._2)
            else
              (Equation(lhs, rhs)::r._1, r._2)
          case Dot(_, _) => (r._1, x:: r._2)
          case _         => (Equation(lhs, rhs) :: r._1, r._2)
        }
        case _ => r
      })
    

    val simplicitOdes = implicitOdes.map(x => (x))
    // Finding all the variables to be solved in the implicit ODEs
    val vars: List[Var] = implicitOdes.map(x => findVars(x.lhs, hashMap)(List.empty) ::: findVars(x.rhs, hashMap)(List.empty)).flatten.distinct
    // Only the highest order variables are variables to be solved, get rid of the lower order ones
    val trueVars = vars.filter(x => !directedVars.contains(x) &&
      !(vars.exists(y => (y.name.x == x.name.x && y.name.primes > x.name.primes))))
    val odes = GE.run(simplicitOdes, trueVars, hashMap).map(x => Continuously(x))
    remainActions ::: explicitOdes ::: odes ::: disODEs
  }
}
