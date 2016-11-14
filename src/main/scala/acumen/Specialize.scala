package acumen

import BindingTimeAnalysis._
import Constants._
import GE._
import Pretty._
import Simplifier._
import SymbolicCommon._
import interpreters.Common._
import annotation.tailrec
import Errors._
import spire.math.Rational
import scala.collection.immutable.ListSet

/**
 * Partial evaluator that specializes annotated program based on BTA result
 *  Assumption:  Equations are reordered based on dependence (DAG)
 *  @param subs: substitution function
 *  @Result: Specialized expression and additional equations due to hash-consing
 */
object Specialization {

  def specialize(aexpr: AExpr[Label], bta: Map[Label, Constraint], env: Map[Expr, Expr], conditionalAction: List[Action], subs: Expr => Expr): (Expr, List[Action]) = {
    val specializeE = specialize(_: AExpr[Label], bta, env, conditionalAction, subs)
    val inlineE = inline(_: Expr, env)
    def specializeEs(es: List[AExpr[Label]]) = {
      val pairs = (for (e <- es) yield specializeE(e)).toList
      val aes = pairs.map(_._1)
      val aas = pairs.foldLeft(List[Action]())((r, x) =>
        x._2 ::: r)
      (aes, aas)
    }
    def get(e: Expr) = if (env.contains(e)) env(e) else e
    // Main pattern match
    aexpr match {
      case ALit(gv, l) => ((Lit(gv)).setPos(aexpr.pos), Nil)
      case AVar(name, l) =>
        val sn = get(subs(Var(name))) match {
          case Lit(sg) => Lit(sg)
          case nonLit  => subs(Var(name))
        }
        (sn.setPos(aexpr.pos), Nil)
      case ATypeOf(cn, l)       => (TypeOf(cn).setPos(aexpr.pos), Nil)
      case ADot(aer, name, l)   => (Dot(aer.expr.setPos(aer.pos), name).setPos(aexpr.pos), Nil)
      case AQuest(aer, name, l) => (Quest(aer.expr, name).setPos(aexpr.pos), Nil)
      case AExprVector(es, l) => specializeEs(es) match {
        case (aes, aas) => (ExprVector(aes).setPos(aexpr.pos), aas)
      }

      case ASum(ae, ai, acol, acond, l) =>
        val i = ai.expr
        val btaNew = bta + ((ai.an, Known(ai.an)))
        specializeE(acol)._1 match {
          case ExprVector(ls) =>
            // Make sum based on cond result and bind i to acol(i)
            val sumes = for (
              bindRhs <- ls;
              if specialize(acond, btaNew, env + ((i, bindRhs)), conditionalAction, subs)._1 == Lit(GBool(true))
            ) yield specialize(ae, btaNew, env + ((i, bindRhs)), conditionalAction, subs)._1
            if (sumes.length > 1)
              (mkBinOp("+", sumes), Nil)
            else if (sumes.length == 1)
              (sumes(0), Nil)
            else (RationalZeroLit, Nil)
          case col => (Sum(specializeE(ae)._1, ai.expr.name, specializeE(acol)._1, specializeE(acond)._1), Nil)
        }
      case AExprLet(bs, expr, l) => {
        // Env plus newRhs       
        val newEnv_newRhs =
          bs.foldLeft((env, List[(Name, Expr)]())) {
            case (r, (bv, bExpr)) =>

              (r._1 + (bv.expr -> specialize(bExpr, bta, r._1, conditionalAction, subs)._1),
                (bv.name, specialize(bExpr, bta, r._1, conditionalAction, subs)._1) :: r._2)
          }
        bta(l) match {
          case Known(_)   => specialize(expr, bta, newEnv_newRhs._1, conditionalAction, subs)
          case Unknown(_) => (ExprLet(newEnv_newRhs._2.reverse, expr.expr), Nil)
        }
      }
      case AIndex(ae, aidx, l) =>
        val (e, es) = (specializeE(ae), specializeEs(aidx))
        val neweqs = e._2 ::: es._2
        (inlineE(e._1), es._1 map inlineE) match {
          case (ExprVector(ls), Lit(GRational(i)) :: Nil) if i.isWhole => (ls(i.toInt), neweqs)
          case (ExprVector(ls), Lit(GRational(i)) :: Lit(GRational(j)) :: Nil) if i.isWhole && j.isWhole =>
            ls(i.toInt) match {
              // Perform vector lookup
              case ExprVector(ls1) => (ls1(j.toInt), neweqs)
              case _               => (Index(e._1, es._1).setPos(aexpr.pos), neweqs)
            }
          case _ => (Index(e._1, es._1).setPos(aexpr.pos), neweqs)
        }
      case ALambda(vs, f, _) => (Lambda(vs, f.expr), Nil)
      case ACall(af, aes, l) =>
        val e = specializeE(af)
        val es = specializeEs(aes)
        val neweqs = e._2 ::: es._2
        (Call(e._1.setPos(af.pos), es._1).setPos(aexpr.pos), neweqs)
      case AExprInterval(ae1, ae2, l) => (ExprInterval(ae1.expr, ae2.expr), Nil)
      case AOp(f, aes, l) =>
        val ses = specializeEs(aes)   
        val (es, newEquation) = (ses._1, ses._2)
        val newenv = env ++ convertEqsEnv(newEquation)
        f.x match {
          case "dif" => es match {
            case e :: Nil =>
              // Time derivative
              val (dt, neweqs) = SD.td(e, env, conditionalAction ::: newEquation).pair
              (dt, neweqs ::: newEquation)
            case e :: v :: Nil => env.getOrElse(v, v) match {
              case Var(n) =>
                // Partial derivative
                val (dif, neweqs) = SD.dif(e, n, env, conditionalAction ::: newEquation).pair
                (dif, neweqs ::: newEquation)
              case ExprVector(ls) =>
                val esd = ls.map(_ match {
                  case Var(n) => (SD.dif(e, n, env, conditionalAction)).pair
                  case _      => throw InvalidSymbolicDifferentiation(v, "partial")
                })
                (ExprVector(esd.map(x => x._1)),
                  esd.foldLeft(List[Action]())((r, x) => r ::: x._2))
              case _ => throw InvalidSymbolicDifferentiation(v, "partial")
            }
            case _ => error("Can't perform symbolic differentiation on " + aexpr)
          }
          case "length" =>
            es(0) match {
              case Var(n) => inline(Var(n), newenv) match {
                case ExprVector(l) => (Lit(GRational(l.length)), newEquation)
                case Lit(_)        => (Lit(GRational(1)), newEquation)
                case _             => (Op(f, es), newEquation)
              }
              case ExprVector(l) => (Lit(GRational(l.length)), newEquation)
              case e             => (Op(f, e :: Nil), newEquation)
            }
          case "_:_:_" =>
            es.map(x => inline(x, newenv)) match {
              case Lit(GRational(n1)) :: Lit(GRational(n2)) :: Lit(GRational(n3)) :: Nil 
                 if n1.isWhole && n2.isWhole && n3.isWhole =>
                val newVector = ExprVector((n1.toInt until (n3.toInt + 1, n2.toInt)).toList map (x => Lit(GRational(x))))
                (newVector,
                  newEquation)
              case other => (Op(f, es), newEquation)
            }
          case _ =>
            // Perform eval function at the value level when all elements are reduced to Lit
            exprsToValues(es.map(x => inline(x, newenv).setPos(x.pos))) match {
              // Invoke the Common.evalop function to evaluate doubles/intervals accordingly
              case Some(ls) =>
                val symbolicEval =   (f.x,ls) match{
                    case ("+", VLit(GRational(n)) :: VLit(GRational(m)) :: Nil) => 
                      Lit(GRational(n+m))
                    case ("-", VLit(GRational(n)) :: VLit(GRational(m)) :: Nil) => 
                      Lit(GRational(n-m))
                    case ("*", VLit(GRational(n)) :: VLit(GRational(m)) :: Nil) => 
                      Lit(GRational(n*m))
                    case ("/", VLit(GRational(n)) :: VLit(GRational(m)) :: Nil) => 
                      Lit(GRational(n/m))
                    case ("^", VLit(GRational(n)) :: VLit(GRational(m)) :: Nil) if m.denominator == 1 => 
                      Lit(GRational(n.pow(m.toInt)))
                    case _ => mkOp(f.x, es:_*).setPos(aexpr.pos)
                  }
                  (symbolicEval, newEquation)
              // Symbolic vector-vector operator evaluation
              case _ => (f.x, es.map(x => inline(x, newenv))) match {
                case (op, ExprVector(ls) :: Nil) => (symUnaryVectorOp(op, ExprVector(ls)), newEquation)
                case ("inv", ExprVector(l1) :: ExprVector(l2) :: Nil) =>
                  (symUnaryVectorOp("inv", ExprVector(ExprVector(l1) :: ExprVector(l2) :: Nil)), newEquation)
                case (op, Lit(n) :: ExprVector(ls) :: Nil) =>
                  (symBinScalarVectorOp(op, Lit(n), ExprVector(ls)), newEquation)
                case (op, ExprVector(ls) :: Lit(n) :: Nil) =>
                  (symBinVectorScalarOp(op, ExprVector(ls), Lit(n)), newEquation)
                case (op, ExprVector(ls1) :: ExprVector(ls2) :: Nil) =>
                  (symBinVectorOp(op, ExprVector(ls1), ExprVector(ls2)), newEquation)
                case _ => (mkOp(f.x, es:_*).setPos(aexpr.pos), newEquation)
              }
            }
        }
    }
  }

  def specialize(aaction: AAction[Label], bta: Map[Label, Constraint], env: Map[Expr, Expr], dvars: List[Var],
                 conditionalAction: List[Action], subs: Expr => Expr): (List[Action], Map[Expr, Expr], List[Action]) = {
    val specializeE = specialize(_: AExpr[Label], bta, env, conditionalAction, subs)
    val inlineE = inline(_: Expr, env)
    aaction match {
      case AContinuously(c, l) =>
        val result = specialize(c, bta, env, conditionalAction, subs)
        (List(result._1), result._2, result._3)
      case ADiscretely(c, l) => c match {
        case AAssign(lhs, rhs, l1) => lhs match {
          case AVar(_, _) => (List(Discretely(Assign(lhs.expr.setPos(lhs.pos), rhs.expr))), env, Nil)
          case _          => (List(Discretely(Assign(specializeE(lhs)._1, specializeE(rhs)._1))), env, Nil)
        }
        case ACreate(x, name, args, l) => (List(Discretely(Create(x, name, args.map(x => specializeE(x)._1)))), env, Nil)
        case AElim(ae, l)              => (List(Discretely(Elim(ae.expr))), env, Nil)
        case AMove(aobj, ap, l)        => (List(Discretely(Move(aobj.expr, ap.expr))), env, Nil)
      }
      case AForEach(i, q, aes, l) => {
        inlineE(specializeE(q)._1) match {
          case ExprVector(es) =>
            val sActions_neqs = (0 to es.length - 1).toList.foldLeft((List[Action](), List[Action]())) {
              case ((sactions, neqs), j) =>
                val newEnv = env + (Var(i) -> es(j))
                val newactions = aes.map(a =>
                  specialize(a, bta, newEnv, dvars, conditionalAction, mksub(Var(i), es(j)) compose subs))
                ((newactions.map(x => x._1)).flatten.reverse ::: sactions,
                  newactions.map(x => x._3).flatten ::: neqs)
            }
            (sActions_neqs._1, env, sActions_neqs._2.distinct)
          case eq =>
            val newActions = (aes.map(a => specialize(a, bta, env, dvars, conditionalAction, mksub(Var(i), Var(i)))._1)).flatten
            (ForEach(i, eq, newActions) :: Nil, env, Nil)
        }
      }
      case AIfThenElse(cond, t, e, l, scope1, scope2) =>
        val newcond = specializeE(cond)
        val newt = specializeListActions(t, bta, env, dvars, conditionalAction)
        val newe = specializeListActions(e, bta, env, dvars, conditionalAction)
        bta(cond.an) match {
          case Known(_) => inlineE(newcond._1) match {
            case Lit(GBool(true))  => (newt._1, env, newt._3)
            case Lit(GBool(false)) => (newe._1, env, newe._3)
          }
          case Unknown(_) => (List(IfThenElse(newcond._1,
            newt._1,
            newe._1)),
            env, (newcond._2 :::
              newt._3 ::: newe._3).distinct)
        }
      case ASwitch(cond, clauses, l) => {
        val sclausesTuple = clauses.map { c =>
          val sc = specializeListActions(c.rhs, bta, env, dvars, conditionalAction)
          (Clause(c.lhs, specializeE(c.assertion)._1, sc._1), sc._3)
        }
        // specialized clauses and any newly added equations from hash-consing
        val (scalauses, neqs) = sclausesTuple.unzip
        (List(Switch(specializeE(cond)._1,
          scalauses)),
          env, neqs.flatten)
      }
      case AClaim(aq, l) =>
        (List(Claim(aq.expr)), env, Nil)
      case AHypothesis(s, aq, l) =>
        (List(Hypothesis(s, aq.expr)), env, Nil)
    }
  }
  // Specialized equation, return result and augmented Env
  def specialize(aequation: AnContinuousAction[Label], bta: Map[Label, Constraint], env: Map[Expr, Expr],
                 conditionalAction: List[Action], subs: Expr => Expr): (Action, Map[Expr, Expr], List[Action]) = {
    aequation match {
      case AEquation(lhs, rhs, l) =>
        val (newrhs, rhseqs) = specialize(rhs, bta, env, conditionalAction, subs)
        lhs match {
          // For directed equations
          case AVar(name, lv) => newrhs match {
            case e => (Continuously(Equation(lhs.expr.setPos(lhs.pos), e)), env + (lhs.expr -> e), rhseqs)
          }

          case _ =>
            val (newrhs, rhseqs) = specialize(rhs, bta, env, conditionalAction, subs)
            val (newlhs, lhseqs) = specialize(lhs, bta, env, conditionalAction ::: rhseqs, subs)
            (Continuously(Equation(newlhs, newrhs)), env, (rhseqs ::: lhseqs).distinct)
        }
    }
  }

  def specializeListActions(aActions: List[AAction[Label]],
                            bta: Map[Label, Constraint], scopeEnv: Map[Expr, Expr],
                            dvars: List[Var], conditionalAction: List[Action]): (List[Action], Map[Expr, Expr], List[Action]) = {
    val equations = aActions.filter(x => x.isInstanceOf[AContinuously[Label]]).map(y => y match {
      case AContinuously(c, l) => c
    }).asInstanceOf[List[AEquation[Label]]]
    val directedEquations = equations.filter(x => x.lhs.isInstanceOf[AVar[_]] || x.lhs.isInstanceOf[AIndex[_]])
    val indirectedEquations = equations.filter(x => !x.lhs.isInstanceOf[AVar[_]] && !x.lhs.isInstanceOf[AIndex[_]])
    val otherActions = aActions.filter(x => !x.isInstanceOf[AContinuously[Label]])

    // For every directed variables, regard its lower derivatives also as "solved"
    val directedVars = aActions.map(findDirectedVars).flatten.foldLeft(List[Var]())((r, x) => x match {
      case Var(Name(n, p)) =>
        if (p > 0)
          (for (i <- 0 to p) yield Var(Name(n, i))).toList ::: r
        else
          x :: r
    })
    // First specialize all directed equations (topologically sort them first)  
    val orderLabels = Specialization.topoSort(directedEquations)
    val orderedEquations = orderLabels.reverse.foldLeft(List[AEquation[Label]]())((r, x) =>
      equations.filter(e => e.an == x) ::: r).map(x => AContinuously(x, x.an))
    val (resultDirecdOde, envAfterOde, neweqsAfterOde) =
      orderedEquations.foldLeft((List[Action](), scopeEnv, List[Action]())) { (r, x) =>
        val sode = specialize(x, bta, r._2, dvars ::: directedVars, r._3 ::: conditionalAction, nosub)
        (sode._1 ::: r._1, sode._2 ++ r._2, sode._3 ::: r._3)
      }
    val odeeqs = resultDirecdOde ::: neweqsAfterOde
    // Specialize non-equation actions    
    val r = otherActions.foldLeft((List[Action](), List[Action](), neweqsAfterOde, List[Action]())) { (r, x) =>
      val (sactions, newenv, eqs) = specialize(x, bta, envAfterOde, dvars ::: directedVars, odeeqs ::: conditionalAction, nosub)
      sactions match {
        case IfThenElse(_, _, _) :: n =>
          (r._1, sactions ::: r._2, eqs ::: r._3, r._4)
        case Switch(_, _) :: n =>
          (r._1, sactions ::: r._2, eqs ::: r._3, r._4)
        case Continuously(Equation(_, _)) :: n =>
          (sactions ::: r._1, r._2, r._3, eqs ::: r._4)
        case Discretely(Assign(_, _)) :: n =>
          (sactions ::: r._1, r._2, r._3, eqs ::: r._4)
        case _ =>
          (r._1, sactions ::: r._2, eqs ::: r._3, r._4)
      }
    }
    val Iodes = r._1; val Ifs = r._2; val neweqsinIf = r._3; val neweqsoutIf = r._4
    // Specialize undirected equations   
    val (specializedIndirecedEquations, envAfterGE, neweqsGE) =
      indirectedEquations.foldLeft((List[Action](), envAfterOde, List[Action]())) { (r, x) =>
        val (aaction, ev, eqs) = specialize(x, bta, envAfterOde, odeeqs ::: conditionalAction, nosub)
        (aaction :: r._1, ev ++ r._2, eqs ::: r._3)
      }
    
    // Calling GE to directed all undirected equations
    val bindings = convertEqsEnv(neweqsGE ::: neweqsAfterOde ::: neweqsoutIf)
    val directedVars1 = bindings.map(x => x._1).foldLeft(List[Var]())((r, x) => x match {
      case Var(Name(n, p)) =>
        if (p > 0)
          (for (i <- 0 to p) yield Var(Name(n, i))).toList ::: r
        else
          Var(Name(n, p)) :: r
    }).union(directedVars)
    val inlinedIndirectedEqs = (specializedIndirecedEquations.map(inline(_,envAfterGE))::: Iodes)
    val (geEqs, hashGE) = BindingTimeAnalysis.gaussianElimination(inlinedIndirectedEqs, dvars ::: directedVars1, bindings) 
    ((geEqs:::hashGE::: Ifs ::: odeeqs ::: neweqsGE.distinct).reverse ::: (neweqsAfterOde ::: neweqsGE ::: neweqsoutIf).distinct, envAfterOde, (neweqsAfterOde ::: neweqsGE ::: neweqsinIf ::: neweqsoutIf:::hashGE).distinct)
  }

  // Graph structure for topological sorting
  case class Graph(vs: List[Vertex])
  case class Edge(val from: Label, val dest: Label)
  case class Vertex(val label: Label, val edges: List[Edge]) {
    def addEdge(e: Edge) = Vertex(label, e :: edges)
    def deleteEdge(e: Edge) = Vertex(label, edges.filter(x => x != e))
    def hasNoIncomingEdges: Boolean =
      !edges.exists(x => x.dest == label)
    override def toString = label.toString + "<-" + "( " +
      edges.map(x => x.toString).mkString(", ") + ")"
  }
  // Topological sort a list of equations, based on their dependence relations
  // Assumption: All the equations are statically known, that is, no variables in those equations
  // which don't have a assignment equation to calculate it
  def topoSort(es: List[AEquation[Label]]): List[Label] = {
    val directedVars = es.map(x => x.lhs match {
      case AVar(n, l) => List(AVar(n, l))
      case _          => Nil
    }).flatten
    def findAVars(aexpr: AExpr[Label])(implicit exceptVars: List[AVar[Label]]): List[AVar[Label]] = aexpr match {
      case ALit(n, l) => List.empty
      case AVar(n, l) => if (!exceptVars.exists(x => x.name == n)) List(AVar(n, l)) else List.empty
      case AOp(f, args, l) => f match {
        case Name("dif", 0) => args match {
          case e :: v :: Nil =>
            val varse = findAVars(e).map(v =>
              v :: (es.filter(x => x.lhs match {
                case AVar(m, _) => m.x == v.name.x
                case _          => false
              }).map(y => findAVars(y.lhs) ::: findAVars(y.rhs))).flatten).flatten
            val morevars = varse ::: varse.map(y => directedVars.filter(_.name.x == y.name.x)).flatten
            morevars ::: findAVars(v)
          case _ => args.map(findAVars(_)).flatten.distinct
        }
        case _ => args.map(findAVars(_)).flatten.distinct
      }

      // Explicit vector is considered knwon, thus will not add depenence edge to graph(fix me)
      case AExprVector(es, _) => es.map(findAVars(_)).flatten.distinct
      case AIndex(e, idx, an) => (findAVars(e) ::: idx.map(findAVars(_)).flatten).distinct
      case ACall(af, aes, an) => (findAVars(af) ::: aes.map(findAVars(_)).flatten).distinct
      case ASum(e, i, col, cond, an) =>
        List(e, col, cond).map(findAVars(_)(i :: exceptVars)).flatten.distinct
      case AExprLet(bs, e, an) => findAVars(e)(bs.map(x => x._1) ::: exceptVars)
      case ADot(_, _, _)       => Nil
      case _                   => Nil
    }
    def mkEdges(vs: List[AVar[Label]], env: Map[Var, Label], label: Label): List[Edge] = {
      vs.map(x => env.get(x.expr) match {
        case Some(l) => if (l != label) Edge(l, label) else None
        case None    => None //error("variable " + x + " is unknown")
      }).filterNot(_ == None).asInstanceOf[List[Edge]]
    }

    def equationsToGraph(es: List[AEquation[Label]]) = {
      // Variables to their assigning equation's label
      val env: Map[Var, Label] = es.map(x => x.lhs match {
        case AVar(n, l) => List((Var(n), x.an))
        case _          => Nil
      }).flatten.toMap
      // Each equation is a vertex
      val vertexes: Map[Label, Vertex] = es.map(x => x match {
        case AEquation(lhs, rhs, l) => (l, Vertex(l,
          mkEdges(findAVars(rhs)(List.empty), env, l) :::
            mkEdges(findAVars(lhs)(List.empty), env, l)))
      }).toMap
      vertexes.values.toList
    }

    /* Topological sort algorithm
     * @param S input nodes
     * @param L output sorted labels*/
    @tailrec def topSort(graph: List[Vertex], S: Set[Vertex], L: List[Label]): (List[Vertex], Set[Vertex], List[Label]) = {
      def normalize(n: Vertex, m: Vertex, g: List[Vertex], s: Set[Vertex]): (List[Vertex], Set[Vertex]) = {
        if (m.edges.exists(x => x.from == n.label)) {
          val edge = m.edges.find(e => e.from == n.label).get
          // Delete edge n -> m
          val newG = g.updated(g.indexOf(m), m.deleteEdge(edge))
          val newS = (m.deleteEdge(edge).hasNoIncomingEdges) match {
            case true => s + m
            case _    => s
          }
          (newG, newS)
        } else
          (g, s)
      }
      // Recursive call 
      S.toList match {
        // we are dRationalOneLit
        case Nil => (graph, S, L)
        // S still contains vertex to process
        case _ =>
          val n = S.head
          val dS = S.drop(1)
          val newL = n.label :: L
          val newGS = graph.foldLeft((graph, dS)) {
            case ((newG, newS), m) =>
              normalize(n, m, newG, newS)
          }
          topSort(newGS._1, newGS._2, newL)
      }
    }

    val graph: List[Vertex] = equationsToGraph(es)
    // List of vertexes with no incoming edges
    val S: Set[Vertex] = graph.filter(x => x.hasNoIncomingEdges).toSet
    // run top sort
    val (sortedGraph, _, sortedLabels) = topSort(graph, S, List[Label]())
    // graph has edges => at least RationalOneLit cycle
    if (sortedGraph.exists(x => x.edges.length != 0)) {
      error("Has cycle in graph")
    } else
      sortedLabels.reverse
  }

  // Test whether all expr in list are known litertures
  def exprsToValues(exprs: List[Expr]): Option[List[VLit]] = {
    val lits: List[Lit] = exprs.filter(e => e.isInstanceOf[Lit]).asInstanceOf[List[Lit]]
    val values = lits.map(x => x match {
      case Lit(gv) => VLit(gv).setPos(x.pos)
    })
    if (values.size == exprs.length)
      Some(values)
    else
      None
  }
  // Symbolic differentiation
  import SD._

  def nosub = (x: Expr) => x
  def mksub(v: Expr, e: Expr): Expr => Expr =
    (x: Expr) => if (x == v) e else nosub(x)

  // Find AVars in the expr

  def findVars(expr: Expr, bindings: Map[Expr, Expr])(implicit exceptVars: List[Var]): List[Var] = expr match {
    case Lit(n) => Nil
    case Var(n) =>
      if (exceptVars.contains(Var(n)) || n.x == "pi")
        Nil
      else {
        if (bindings.contains(Var(n)))
          findVars(bindings(Var(n)), bindings)
        else
          List(Var(n))
      }

    case Op(f, es)            => es.map(findVars(_, bindings)).flatten.distinct
    case ExprVector(ls)       => ls.map(findVars(_, bindings)).flatten.distinct
    case Index(e, idx)        => (findVars(e, bindings) ::: idx.map(findVars(_, bindings)).flatten).distinct
    case Sum(e, i, col, cond) => List(e, col, cond).map(findVars(_, bindings)(Var(i) :: Nil)).flatten.distinct
    case ExprLet(bs, e)       => findVars(e, bindings)(bs.map(x => Var(x._1)) ::: exceptVars)
    case Dot(_, _)            => Nil
  }
  def findDirectedVars(a: AAction[Label]): List[Var] = {
    a match {
      case AContinuously(e @ AEquation(lhs, rhs, _), _) => lhs match {
        case AVar(n, _) => List(Var(n))
        case _          => Nil
      }
      case AIfThenElse(_, t, e, _, _, _) =>
        val vt = t.map(findDirectedVars).flatten
        val ve = e.map(findDirectedVars).flatten
        vt.filter(x => ve.contains(x))
      case _ => Nil
    }
  }

  def convertEqsEnv(eqs: List[Action]): Map[Expr, Expr] =
    eqs.foldLeft(Map[Expr, Expr]())((r, x) => x match {
      case Continuously(Equation(lhs, rhs)) => r + (lhs -> rhs)
      case _                                => r
    })
  def inline(a: Action, env: Map[Expr, Expr]): Action = a match {
    case Continuously(Equation(lhs, rhs)) => Continuously(Equation(inline(lhs, env), inline(rhs, env)))
    case _                                => a
  }
  
  def convertDots(e: Expr): Expr = e match {
    case Dot(d, s)      => Var(s)
    case ExprVector(ls) => ExprVector(ls map convertDots)
    case Op(f, es)      => Op(f, es.map(convertDots(_)))
    case _              => e
  }
  
  def findDots(e: Expr): List[Dot] = e match {
    case Dot(d, s)      => List(Dot(d, s))
    case ExprVector(ls) => ls.map(findDots(_)).flatten.distinct
    case Op(f, es)      => es.map(findDots(_)).flatten.distinct
    case _              => Nil
  }

  def inline(e: Expr, env: Map[Expr, Expr]): Expr = e match {
    case Lit(_) => e
    case Var(n) =>
      if (env.contains(Var(n)))
        inline(env(e), env)
      else
        e
    case ExprVector(ls) => ExprVector(ls.map(inline(_, env)))
    case Op(f, es)      => Op(f, es.map(inline(_, env)))
    case _              => e
  }
  def findConnectedAction(vars: ListSet[Var],
                          ceqs: List[Action],
                          eqs: Map[Action, ListSet[Var]]): (ListSet[Var], List[Action], Map[Action, ListSet[Var]]) = {
    val newCeqs = eqs.filter { case (eq, vs) => vs.intersect(vars) != ListSet.empty }
    // We found all equations connected with eq
    if (newCeqs.size == 0) {
      (vars, ceqs, eqs)
    } else {
      val newVars = newCeqs.foldLeft(vars)((r, x) => r ++ x._2)

      val newEqs = newCeqs.foldLeft(ceqs)((r, x) => x._1 :: r)
      findConnectedAction(newVars, newEqs, eqs -- newCeqs.map(x => x._1))
    }
  }

  def findConnectedActions(eMap: Map[Action, ListSet[Var]]): List[List[Action]] = {
    if (eMap.size > 0) {
      val head = eMap.head
      val (_, connectedEqsHead, remainMap) = findConnectedAction(head._2, head._1 :: Nil, eMap - head._1)
      connectedEqsHead :: findConnectedActions(remainMap)
    } else
      Nil
  }

  def connectedEquations(es: List[Action]): List[List[Action]] = {
    val eqMap = es.foldLeft(Map[Action, ListSet[Var]]())((r, x) =>
      r + ((x, ListSet.empty ++ (findVarsTCasualize(x)))))
    findConnectedActions(eqMap)
  }
  def findVarsToDirect(e: Expr, directedVars: List[Var]): Option[List[Var]] = {
    e match {
      case Op(Name("norm", 0), es) => Some(Nil)
      case Op(Name("sin", 0), es)  => Some(Nil)
      case Op(Name("cos", 0), es)  => Some(Nil)
      case Op(Name("asin", 0), es) => Some(Nil)
      case Op(Name("acos", 0), es) => Some(Nil)
      case Op(Name("dot", 0), es)  => Some(Nil)
      // We don't try to calsualize symbolic diff expressions
      case Op(Name("dif", 0), es)  => None
      case Op(f, es) =>
        val vars = es.map(x => findVarsToDirect(x, directedVars))
        if (vars.exists(x => x match { case None => true; case _ => false })) None
        else {
          Some(es.foldLeft(List[Var]())((r, x) => findVarsToDirect(x, directedVars).get ::: r))
        }

      case _ => Some(findVars(e, Map.empty)(directedVars))
    }
  }
  def findVarsTCasualize(a: Action): List[Var] = a match {
    case Continuously(Equation(lhs, rhs)) => findVars(lhs, Map.empty)(Nil) ::: findVars(rhs, Map.empty)(Nil)
    case ForEach(_, _, body)              => body.foldLeft(List[Var]())((r, x) => r ::: findVarsTCasualize(x))
    case IfThenElse(_, t, e) => t.foldLeft(List[Var]())((r, x) => r ::: findVarsTCasualize(x)) intersect
      e.foldLeft(List[Var]())((r, x) => r ::: findVarsTCasualize(x))
    case _ => Nil
  }

  def findVarsEs(es: List[Expr], dVars: List[Var]): List[Var] = es.foldLeft(List[Var]())((r, x) => findVars(x, Map.empty)(dVars) ::: r)
  def findVarsTCasualize(e: Expr, directedVars: List[Var]): List[Var] = {
    findVars(e, Map.empty)(directedVars)
  }
  // Initialize bigraph
  def initBigraph(es: List[Equation], directedVars: List[Var]): (Map[Equation, ListSet[Var]], Map[Var, ListSet[Equation]], List[Equation]) = {
    es.foldLeft((Map[Equation, ListSet[Var]](),
      Map[Var, ListSet[Equation]](), List[Equation]())) {
      case ((eqs, vars, eqsLaterStage), x) => x match {
        case Equation(lhs, rhs) =>
          (findVarsToDirect(lhs, directedVars), findVarsToDirect(rhs, directedVars)) match {
            // No symbolic differentiation operator on either sides
            case (Some(v1), Some(v2)) =>
              val varsInside = ListSet.empty ++ (v1 ::: v2)
              val newVars = varsInside.foldLeft(vars) {
                case (r, v) =>
                  r.contains(v) match {
                    case false => r + ((v, ListSet(x)))
                    case _ =>
                      r.updated(v, r(v) + x)
                  }
              }
              (eqs + ((x, varsInside)),
                newVars, eqsLaterStage)
            // This Equation needs to be handled after apply symbolic differentiation
            case _ => (eqs, vars, x :: eqsLaterStage)
          }

      }
    }
  }
  // Tarjan's causalization algorithm
  def tarjan(es: List[Equation], directedVars: List[Var], env: Map[Expr, Expr]): (List[Equation], List[Equation], List[Var]) = {

    // Build a bigraph with equations and variables connected
    case class Bigraph(val eqs: Map[Equation, ListSet[Var]],
                       val vars: Map[Var, ListSet[Equation]],
                       val casualEquations: Map[Int, (Equation, Var)],
                       // the ordering numbers
                       val low: Int, val high: Int) {

      // Remove RationalOneLit edge between the variable and equation 
      def updateEquation(eqs: Map[Equation, ListSet[Var]],
                         v: Var, eq: Equation) =
        eqs.contains(eq) match {
          case true =>
            eqs.updated(eq, eqs(eq) - v)
          case _ => eqs
        }

      def updateVariable(vars: Map[Var, ListSet[Equation]],
                         eq: Equation, v: Var) =
        vars.contains(v) match {
          case true => vars.updated(v, vars(v) - eq)
          case _    => vars
        }

      /* If an acasual equation has only RationalOneLit variable, remove that variable 
       * from the equations it points at, number the equation with low
       */
      def firstRule(input: Bigraph): Bigraph = {
        input.eqs.toList.find { case (e, vs) => vs.size == 1 } match {
          case Some((e, vs)) =>
            val newCasual = input.casualEquations + ((input.low, (e, vs.head)))
            val newEqs = input.vars(vs.head).foldLeft(input.eqs - e)((r, x) =>
              updateEquation(r, vs.head, x))
            val newVars = input.vars - vs.head
            Bigraph(newEqs, newVars, newCasual, input.low + 1, input.high)
          case None => input
        }
      }

      /* If a variable only appear in RationalOneLit equation, remove that equation from all other
       * variable list and number the equation with high
       */
      def secondRule(input: Bigraph): Bigraph = {
        input.vars.toList.find { case (v, es) => es.size == 1 } match {
          case Some((v, es)) =>
            val newCasual = input.casualEquations + ((input.high, (es.head, v)))
            val newVars = input.eqs(es.head).foldLeft(input.vars - v)((r, x) =>
              updateVariable(r, es.head, x))
            val newEqs = input.eqs - es.head
            Bigraph(newEqs, newVars, newCasual, input.low, input.high - 1)
          case None => input
        }
      }
      /* Apply tarjan's two rules until reaches a fixed point */
      def normalize(input: Bigraph): Bigraph = {
        val newBigraph = (firstRule(input))
        newBigraph.casualEquations.size == input.casualEquations.size match {
          case false => normalize(newBigraph)
          case _ => {
            newBigraph
          }
        }
      }
     
      def normalizeSelf = normalize(this)
    }

    def hasVar(ae: Expr, v: Var): Boolean =
      findVars(ae, Map.empty)(directedVars).exists { x => x.name == v.name }

    /* Normalize an expr to the form (coef*v, constant) */
    def causalizeExpr(ae: Expr, v: Var): (Expr, Option[Expr]) =
      ae match {
        case Lit(_)         => (RationalZeroLit, Some(ae))
        case Lambda(_, _)   => (RationalZeroLit, Some(ae))
        case ExprVector(es) => (RationalZeroLit, Some(ae))
        case ExprLet(bs, e) => (RationalZeroLit, Some(ae))
        case Dot(_, _)      => (RationalZeroLit, Some(ae))
        case Var(n) =>
          if (n == v.name) (RationalOneLit, None)
          else (RationalZeroLit, Some(ae))
        case Op(f, es) =>
          if (!hasVar(ae, v))
            (RationalZeroLit, Some(ae))
          else (f, es) match {
            case (Name("+", _), e1 :: e2 :: nil) => {
              val ((coe1, cons1), (coe2, cons2)) = (causalizeExpr(e1, v), causalizeExpr(e2, v))
              val coe = mkOp("+", coe1, coe2)
              val cons = (cons1, cons2) match {
                case (Some(c1), None)     => Some(c1)
                case (None, Some(c2))     => Some(c2)
                case (Some(c1), Some(c2)) => Some(Op(Name("+", 0), c1 :: c2 :: Nil))
                case (None, None)         => None
              }
              (coe, cons)
            }
            case (Name("-", _), e1 :: e2 :: nil) => {
              val ((coe1, cons1), (coe2, cons2)) = (causalizeExpr(e1, v), causalizeExpr(e2, v))
              val coe = mkOp("-", coe1, coe2)
              val cons = (cons1, cons2) match {
                case (Some(c1), None)     => Some(c1)
                case (None, Some(c2))     => Some(mkOp("-", RationalZeroLit, c2))
                case (Some(c1), Some(c2)) => Some(mkOp("-", c1, c2))
                case (None, None)         => None
              }
              (coe, cons)
            }
            case (Name("*", _), e1 :: e2 :: nil) => {
              val ((coe1, cons1), (coe2, cons2)) = (causalizeExpr(e1, v), causalizeExpr(e2, v))
              val coe = (coe1, coe2) match {
                case (c1, Lit(GInt(0))) => c1
                case (Lit(GInt(0)), c2) => c2
                case (c1, c2)           => throw causalizeExprFail(ae, v)
              }
              val cons = (cons1, cons2) match {
                case (Some(c1), None) => Some(mkOp("*", coe, c1))
                case (None, Some(c2)) => Some(mkOp("*", coe, c2))
                case (Some(c1), Some(c2)) =>
                  Some(mkOp("*", coe, mkOp("*", c1, c2)))
                case (None, None) => Some(coe)
              }
              (cons.get, None)
            }
            case (Name("/", _), e1 :: e2 :: nil) => {
              // Variable can only appear in the nominator
              if (hasVar(e2, v))
                throw causalizeExprFail(ae, v)
              else {
                val (coe1, cons1) = causalizeExpr(e1, v)
                (mkOp("/", coe1, e2), Some(mkOp("/", cons1.get, e2)))
              }
            }
            case _ => (RationalZeroLit, Some(ae))
          }
        case _ => (RationalZeroLit, Some(ae))
      }
    /* Use this equation to solve variable v (causalized form) */
    def causalizeEquation(eq: Equation, v: Var): Equation = {
      def printE(e: Equation): String =
        pprint(e.lhs) + " = " + pprint(e.rhs)
      val ((coe1, cons1), (coe2, cons2)) =
        try { (causalizeExpr(eq.lhs, v), causalizeExpr(eq.rhs, v)) }
        catch {
          case causalizeExprFail(e, v) => throw causalizeFail(eq, v, e)
        }
      (cons1, cons2) match {
        case (Some(c1), None) =>
          val coe = mkOp("-", coe2, coe1)
          Equation(v, mkOp("/", c1, coe))
        case (None, Some(c2)) =>
          val coe = mkOp("-", coe1, coe2)
          Equation(v, mkOp("/", c2, coe))
        case (Some(c1), Some(c2)) =>
          val coe = mkOp("-", coe1, coe2)
          val const = mkOp("-", c2, c1)
          Equation(v, mkOp("/", const, coe))
      }
    }

    def lift(e: Expr, an: Label): AExpr[Label] = e match {
      case Lit(n)    => ALit(n, an)
      case Var(n)    => AVar(n, an)
      case Op(f, es) => AOp(f, es.map(lift(_, an)), an)
    }

    val (eMap, vMap, eqsWithPD) = initBigraph(es, directedVars)
    def printE(e: Equation): String =
      pprint(e.lhs) + " = " + pprint(e.rhs)

    val bigraph = Bigraph(eMap, vMap, Map[Int, (Equation, Var)](),
      1, es.length)
    val finalBigraph = bigraph.normalizeSelf
    val causalizedEquations = finalBigraph.casualEquations.toList.sortWith(_._1 < _._1)
    val acausalizedEquations = finalBigraph.eqs.toList.map(_._1)
    val aequationWithVar = causalizedEquations.map(x => (x._2._1,
      x._2._2.asInstanceOf[Var]))
    val causalizedVars = aequationWithVar.map(_._2)
    val mkcasuals = aequationWithVar.map(x => causalizeEquation(x._1, x._2))
    val newEnv = env ++ mkcasuals.map(x => (x.lhs, x.rhs))
    val newDirectedVars = directedVars ::: causalizedVars
    val eqsApplyPD = eqsWithPD
    if (eqsApplyPD != eqsWithPD) {
      val (newc, newac, cv) = tarjan(acausalizedEquations ::: eqsApplyPD, newDirectedVars, newEnv)
      (mkcasuals ::: newc, newac, cv)
    } else
      (mkcasuals, acausalizedEquations ::: eqsApplyPD, causalizedVars)

  }
}