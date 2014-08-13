package acumen
package interpreters

import util.Names._
import util.Conversions._
import scala.math._
import Errors._
import util.Canonical._

//
// Common stuff to CStore Interpreters
//

object Common {
 
  type Env = Map[Name, CValue]
  
  /* get self reference in an env */
  def selfCId(e:Env) : CId =
    e(self) match {
      case VObjId(Some(a)) => a
      case _ => throw ShouldNeverHappen()
    }
  
  /* get the definition associated with a classname in p */
  def classDef(c:ClassName, p:Prog) : ClassDef =
    p.defs.find(_.name == c) match {
      case Some(cl) => cl
      case None => throw ClassNotDefined(c)
    }
 
  /* purely functional unary operator evaluation 
   * at the ground values level */
  def unaryGroundOp(f:String, vx:GroundValue) = {
    def implem(f:String, x:Double) = f match {
        case "sin" => sin(x)
        case "cos" => cos(x)
        case "tan" => tan(x)
        case "acos"=> acos(x)
        case "asin"=> asin(x)
        case "atan"=> atan(x)
        case "toRadians"  => toRadians(x)
        case "toDegrees"  => toDegrees(x)
        case "exp"  => exp(x)
        case "log"  => log(x)
        case "log10"  => log10(x)
        case "sqrt" => sqrt(x)
        case "cbrt" => cbrt(x)
        case "ceil"=> ceil(x)
        case "floor"=> floor(x)
        case "rint"=> rint(x)
        case "round"=> round(x)
        case "sinh" => sinh(x)
        case "cosh" => cosh(x)
        case "tanh" => tanh(x)
        case "signum"=> signum(x)

// Should abs and and - above?

    }
    (f, vx) match {
      case ("not", GBool(x))   => GBool(!x)
      case ("abs", GInt(i))    => GInt(abs(i))
      case ("-",   GInt(i))    => GInt(-i)
      case ("abs", GDouble(x)) => GDouble(abs(x))
      case ("-",   GDouble(x)) => GDouble(-x)
      case ("round", GDouble(x)) => GInt(x.toInt)
      case _                   => GDouble(implem(f, extractDouble(vx)))
    }
  }
  
  /* purely functional binary operator evaluation 
   * at the ground values level */
  def binGroundOp(f:String, vx:GroundValue, vy:GroundValue) = {
    def implem1(f:String, x:Int, y:Int) = f match {
      case "+" => x + y
      case "-" => x - y
      case "*" => x * y
      case "<<" => x << y
      case ">>" => x >> y
      case "&"  => x & y
      case "|"  => x | y
      case "%"  => x % y
      case "xor" => x ^ y
    }
    def implem2(f:String, x:Double, y:Double) = f match {
      case "+" => x + y
      case "-" => x - y
      case "*" => x * y
      case "^" => pow(x,y)
      case "/" => x / y
      case "atan2" => atan2(x,y)
      case _ => throw UnknownOperator(f)
    }
    def implem3(f:String, x:Int, y:Int) = f match {
      case "<"  => x < y
      case ">"  => x > y
      case "<=" => x <= y
      case ">=" => x >= y
    }
    def implem4(f:String, x:Double, y:Double) = f match {
      case "<" => x < y
      case ">" => x > y
      case "<=" => x <= y
      case ">=" => x >= y
    }
    (f, vx, vy) match {
      case (">="|"<="|"<"|">", GInt(n), GInt(m)) => GBool(implem3(f,n,m))
      case ("<"|">"|"<="|">=", _, _) => GBool(implem4(f,extractDouble(vx),extractDouble(vy)))
      case ("+"|"-"|"*"|"<<"|">>"|"&"|"|"|"%"|"xor", GInt(n), GInt(m)) => GInt(implem1(f,n,m))
      case _  => GDouble(implem2(f, extractDouble(vx), extractDouble(vy)))
    }
  }

  def binVectorOp[A](op:String, u:List[Value[_]], v:List[Value[_]]) : Value[A] = {
    lazy val du = extractDoubles(u)
    lazy val dv = extractDoubles(v)
    op match {
      case ".*"  => VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(d1*d2))))
      case "./"  => VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(d1/d2))))
      case ".^"  => VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(pow(d1,d2)))))
      case "+"   => VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(d1+d2))))
      case "-"   => VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(d1-d2))))
      case "dot" => VLit(GDouble(((du,dv).zipped map (_*_)).sum))
      case "cross" =>
        (du, dv) match {
          case (u1::u2::u3::Nil, v1::v2::v3::Nil) => 
            VVector(List(u2*v3 - u3*v2, u3*v1 - u1*v3, u1*v2 - u2*v1) 
                    map (d => VLit(GDouble(d))))
          case _ => throw CrossProductError()
        }
      case _ => throw InvalidVectorVectorOp(op)
    }
  }

  def unaryListOp[A](op:String, u:List[Value[_]]) : Value[A] = {
    lazy val du = extractDoubles(u)
    op match {
      case "length" => VLit(GInt(u.length))
      case _ => throw InvalidListOp(op)
    }
  }

  def unaryVectorOp[A](op:String, u:List[Value[_]]) : Value[A] = {
    lazy val du = extractDoubles(u)
    op match {
      case "length" => VLit(GInt(u.length))
      case "norm" => VLit(GDouble(math.sqrt((du map (d => d*d)).sum)))
      case "floor" => VVector(du map {d => VLit(GDouble(floor(d)))})
      case "ceil" => VVector(du map {d => VLit(GDouble(ceil(d)))})
      case _ => throw InvalidVectorOp(op)
    }
  }

  def binScalarVectorOp[A](op:String, x:GroundValue, u:List[Value[_]]) : Value[A] = {
    op match {
      case "+" => binVectorScalarOp(op,u,x)
      case "*" => binVectorScalarOp(op,u,x)
      case _ => throw InvalidScalarVectorOp(op)
    }
  }

  def binVectorScalarOp[A](op:String, u:List[Value[_]], x:GroundValue) : Value[A] = {
    lazy val dx = extractDouble(x)
    lazy val du = extractDoubles(u)
    op match {
      case "+" => VVector(du map (d => VLit(GDouble(d+dx))))
      case "*" => VVector(du map (d => VLit(GDouble(d*dx))))
      case "/" => VVector(du map (d => VLit(GDouble(d/dx))))
      case ".^" => VVector(du map (d => VLit(GDouble(pow(d,dx)))))
      case _ => throw InvalidVectorScalarOp(op)
    }
  }

  def sequenceOp[A](s:Int, d:Int, e:Int) : Value[A] =
    VVector((s until(e+1,d)).toList map (x => VLit(GInt(x))))
  /* purely functional operator evaluation 
   * at the values level */
  def evalOp[A](op:String, xs:List[Value[_]]) : Value[A] = {
    (op,xs) match {
       case ("==", x::y::Nil) => 
         VLit(GBool(x == y))
       case ("~=", x::y::Nil) => 
         VLit(GBool(x != y))
       case ("_:_:_", VLit(GInt(s))::VLit(GInt(d))::VLit(GInt(e))::Nil) =>
         sequenceOp(s,d,e)
       case (_, VLit(x)::Nil) =>
         VLit(unaryGroundOp(op,x))
       case (_, VList(u)::Nil) =>
         unaryListOp(op,u)
       case (_, VVector(u)::Nil) =>
         unaryVectorOp(op,u)
       case (_,VLit(x)::VLit(y)::Nil) =>  
         VLit(binGroundOp(op,x,y))
       case (_, VVector(u)::VVector(v)::Nil) =>
         binVectorOp(op,u,v)
       case (_, VLit(x)::VVector(u)::Nil) =>
         binScalarVectorOp(op,x,u)
       case (_, VVector(u)::VLit(x)::Nil) =>
         binVectorScalarOp(op,u,x)
       case _ =>
         throw UnknownOperator(op)    
    }
  }

  /* eval Index(e, i) */
  def evalIndexOp[A](e: Value[A], i: Value[A]) : Value[A] = {
    e match {
      case VVector(l) => i match {
        case VLit(GInt(idx)) => try {
          l(idx)
        } catch {
          case _:IndexOutOfBoundsException => throw IndexOutOfBounds(idx)
        }
        case _ => throw ExpectedInteger(i) }
      case _ => throw CantIndex() }
  }

  val magicClassTxt =
    """class Simulator(time, timeStep, outputRows, continuousSkip, endTime, resultType, lastCreatedId) end"""
  val initStoreTxt =
    """#0.0 { className = Simulator, parent = %s, time = 0.0, timeStep = 0.01, 
              outputRows = "WhenChanged", continuousSkip = 0,
              endTime = 10.0, resultType = @Discrete, nextChild = 0,
	      method = "RungeKutta", seed1 = 0, seed2 = 0 }"""

  lazy val magicClass = Parser.run(Parser.classDef, magicClassTxt)
  lazy val initStoreRef = Parser.run(Parser.store, initStoreTxt.format("#0"))
  lazy val initStoreImpr = Parser.run(Parser.store, initStoreTxt.format("none"))
                                  
  // register valid simulator parameters
  val simulatorFields = List("time", "timeStep", "outputRows", "continuousSkip", "endTime", "resultType", "lastCreatedId", "method")

  val specialFields = List("nextChild","parent","className","seed1","seed2")

  def threeDField(name: String) = 
    name == "_3D" || name == "_3DView"
  
  /* error checking */
  
  /**
   * Ensure that for each variable that has an ODE declared in the private section, there is 
   * an equation in scope at the current time step. This is done by checking that for each 
   * primed field name in each object in st, there is a corresponding CId-Name pair in odes.
   */
  def checkContinuousDynamicsAlwaysDefined(prog: Prog, odes: List[(CId, Dot)], st: CStore): Unit = {
    val declaredODENames = prog.defs.map(d => (d.name, (d.fields ++ d.priv.map(_.x)).filter(_.primes > 0))).toMap
    st.foreach { case (o, _) =>
      if (o != magicId(st))
        declaredODENames.get(getCls(o, st)).map(_.foreach { n =>
          if (!odes.exists { case (eo, d) => eo.id == o.id && d.field.x == n.x })
            throw ContinuousDynamicsUndefined(o, n, Pretty.pprint(getObjectField(o, classf, st)), getTime(st))
        })
    }
  }

  /** Check for a duplicate assignment (of a specific kind) scheduled in assignments. */
  def checkDuplicateAssingments(assignments: List[(CId, Dot)], error: Name => DuplicateAssingment): Unit = {
    val duplicates = assignments.groupBy(a => (a._1,a._2)).filter{ case (_, l) => l.size > 1 }.toList
    if (duplicates.size != 0) {
      val first = duplicates(0)
      val x = first._1._2.field
      val poss = first._2.map{case (_,dot) => dot.pos}.sortWith{(a, b) => b < a}
      throw error(first._1._2.field).setPos(poss(0)).setOtherPos(poss(1))
    }
  }
  
  /* runtime checks, should be disabled once we have type safety */

  def checkAccessOk(id:CId, env:Env, st:CStore, context: Expr) : Unit = {
    val sel = selfCId(env)
    lazy val cs = childrenOf(sel, st)
    if (sel != id && ! (cs contains id))
      throw AccessDenied(id,sel,cs).setPos(context.pos)
  }

  def checkIsChildOf(child:CId, parent:CId, st:CStore, context: Expr) : Unit = {
    val cs = childrenOf(parent, st)
    if (! (cs contains child)) throw NotAChildOf(child,parent).setPos(context.pos)
  }

  //
  // ODEs
  // 
  
  class Solver[S <% RichStore[S]](solverName: Value[_], val xs: S, val h: Double)(implicit f: Field[S]) {
    private def msg(meth: String) = "Invalid integration method \"" + meth +
        "\". Please select one of: " + knownSolvers.mkString(", ");
    final def solve: S = {
      solverName match {
        case VLit(GStr(s))              => solveIfKnown(s) getOrElse (throw new Error(msg(s)))
        case VLit(GStr(m))              => throw new Error(msg(m))
        case VClassName(ClassName(c))   => throw new Error(msg(c))
        case m                          => throw new Error(msg(m.toString))
      }
    }
    def knownSolvers = List("EulerForward", "RungeKutta");
    def solveIfKnown(name: String) : Option[S] = name match {
      case "EulerForward" => Some(solveIVPEulerForward(xs, h))
      case "RungeKutta"   => Some(solveIVPRungeKutta(xs, h))
      case _              => None
    }
  }

  def solveIVPEulerForward[S <% RichStore[S]](xs: S, h: Double)(implicit f: Field[S]): S =
    xs +++ f(xs) *** h

  def solveIVPRungeKutta[S <% RichStore[S]](xs: S, h: Double)(implicit f: Field[S]): S = {
    val k1 = f(xs)
    val k2 = f(xs +++ k1 *** (h/2)) 
    val k3 = f(xs +++ k2 *** (h/2))
    val k4 = f(xs +++ k3 *** h)
    xs +++ (k1 +++ k2 *** 2 +++ k3 *** 2 +++ k4) *** (h/6)
  }

  /** Representation of a set of ODEs. */
  abstract class Field[S /* store */] {
    /** Evaluate the field (the RHS of each equation in ODEs) in s. */
    def apply(s: S): S;
  }

  /** Embedded DSL for expressing integrators. */
  abstract class RichStore[S /* store */] {
    def +++(that: S): S;
    def ***(that: Double): S;
  }
  
}
