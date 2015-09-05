package acumen
package interpreters

import TAD.{ TDif }
import util.Names._
import util.Conversions._
import scala.math._
import Errors._
import util.Canonical._
import util.ASTUtil._
import scala.util.parsing.input.Position

//
// Common stuff to CStore Interpreters
//

object Common {
  
  trait Environment[V] extends Any {
    def apply(n: Name): V
    def get(n: Name): Option[V]
    def +(v: (Name,V)): Environment[V]
    def ++(v: Map[Name,V]): Environment[V]
    def empty: Environment[V]
  }
  type Matrix = Array[Array[Double]]

  case class Env(val env: Map[Name, CValue]) extends AnyVal with Environment[CValue] {
    override def apply(n: Name) = env(n)
    override def get(n: Name) = env.get(n)
    override def +(v: (Name,CValue)) = new Env(env + v) 
    override def ++(v: Map[Name,CValue]) = new Env(env ++ v) 
    override def empty = Env.empty
  }
  object Env {
    val empty = new Env(Map.empty)
    def apply(nvs: (Name, Value[CId])*): Env = Env(Map(nvs:_*))
  }
  
  val EulerCromer  = "EulerCromer"
  val EulerForward = "EulerForward"
  val RungeKutta   = "RungeKutta"
  val Taylor       = "Taylor"
  
  /** Get self reference in an env. */
  def selfCId(e:Env) : CId =
    e(self) match {
      case VObjId(Some(a)) => a
      case _ => throw ShouldNeverHappen()
    }
  
  /** Get the definition associated with a classname in p. */
  def classDef(c:ClassName, p:Prog) : ClassDef =
    p.defs.find(_.name == c) match {
      case Some(cl) => cl
      case None => throw ClassNotDefined(c)
    }
  
  /** Convert a relative, qualified name (Dot) d in a given environment env
   *  to an absolute qualified name in a given object (identified by CId). */
  def resolveDot(d: Dot, env: Env, st: CStore): ResolvedDot = d match {
    case Dot(Var(on), n)          => ResolvedDot(extractId(env(on)), d, n)
    case Dot(Dot(Var(pn), on), n) => ResolvedDot(extractId(st(extractId(env(pn)))(on)), d, n)
  }
 
  /** Purely functional unary operator evaluation at the ground values level. */
  def unaryGroundOp(f:String, vx:GroundValue) = {
    def implem(f: String, x: Double) = f match {
      case "-"         => -x
      case "abs"       => abs(x)
      case "sin"       => sin(x)
      case "cos"       => cos(x)
      case "tan"       => tan(x)
      case "acos"      => acos(x)
      case "asin"      => asin(x)
      case "atan"      => atan(x)
      case "toRadians" => toRadians(x)
      case "toDegrees" => toDegrees(x)
      case "exp"       => exp(x)
      case "log"       => log(x)
      case "log10"     => log10(x)
      case "sqrt"      => sqrt(x)
      case "cbrt"      => cbrt(x)
      case "ceil"      => ceil(x)
      case "floor"     => floor(x)
      case "rint"      => rint(x)
      case "round"     => round(x)
      case "sinh"      => sinh(x)
      case "cosh"      => cosh(x)
      case "tanh"      => tanh(x)
      case "signum"    => signum(x)
    }
    def implemIntegral[V: acumen.Integral](f: String, x: V): V = f match {
      case "-" => -x
    }
    def implemReal[V: Real](f: String, x: V): V = f match {
      case "-"         => -x
      case "sin"       => x.sin
      case "cos"       => x.cos
      case "tan"       => x.tan
      case "acos"      => x.acos
      case "asin"      => x.asin
      case "atan"      => x.atan
      case "toRadians" => throw new NotImplemented(f)
      case "toDegrees" => throw new NotImplemented(f)
      case "exp"       => x.exp
      case "log"       => x.log
      case "log10"     => throw new NotImplemented(f)
      case "sqrt"      => x.sqrt
      case "cbrt"      => throw new NotImplemented(f)
      case "ceil"      => throw new NotImplemented(f)
      case "floor"     => throw new NotImplemented(f)
      case "rint"      => throw new NotImplemented(f)
      case "round"     => throw new NotImplemented(f)
      case "sinh"      => throw new NotImplemented(f)
      case "cosh"      => throw new NotImplemented(f)
      case "tanh"      => throw new NotImplemented(f)
      case "signum"    => throw new NotImplemented(f)
    }
    (f, vx) match {
      case ("not", GBool(x))     => GBool(!x)
      case ("abs", GInt(i))      => GInt(abs(i))
      case ("-", GInt(i))        => GInt(-i)
      case ("round", GDouble(x)) => GInt(x.toInt)
      case (f, GIntDif(d))       => f match {
        case "-" => GIntDif(implemIntegral(f, d)) // keep as TDif[Int] if f has an integer result 
        case _   => GDoubleDif(implemReal(f, TDif(d.coeff.map(_.toDouble), d.length))) // otherwise, convert to TDif[Double] 
      }
      case (f, GDoubleDif(d))    => GDoubleDif(implemReal(f, d))
      case _                     => GDouble(implem(f, extractDouble(vx)))
    }
  }
  
  /* purely functional binary operator evaluation 
   * at the ground values level */
  def binGroundOp(f:String, vx:GroundValue, vy:GroundValue): GroundValue = {
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
    def implemIntegral[V: acumen.Integral](f: String, x: V, y: V) = f match {
      case "+" => x + y
      case "-" => x - y
      case "*" => x * y
      case _ => throw UnknownOperator(f)
    }
    def implemReal[V: acumen.Real](f: String, x: V, y: V) = f match {
      case "/" => x / y
      case "^" => x ^ y
      case _ => implemIntegral(f, x, y)
    }
    def toDoubleDif(di: TDif[Int]): TDif[Double] = TDif(di.coeff.map(_.toDouble), di.length)
    (f, vx, vy) match {
      case (">="|"<="|"<"|">", GInt(n), GInt(m)) => GBool(implem3(f,n,m))
      case ("<"|">"|"<="|">=", _, _) => GBool(implem4(f,extractDouble(vx),extractDouble(vy)))
      case ("+"|"-"|"*"|"<<"|">>"|"&"|"|"|"%"|"xor", GInt(n), GInt(m)) => GInt(implem1(f,n,m))
      
      // FIXME Add special case for integer powers of TDif[Int], to avoid lifting to TDif[Double]
      case ("^" | "/", GIntDif(n), GIntDif(m)) => GDoubleDif(implemReal(f, toDoubleDif(n), toDoubleDif(m)))
      case (_, GIntDif(n), GIntDif(m)) => GIntDif(implemIntegral(f, n, m))
      case (_, GDoubleDif(n), GDoubleDif(m)) => GDoubleDif(implemReal(f, n, m))
      
      case (_, GDoubleDif(n), GIntDif(m)) => GDoubleDif(implemReal(f, n, toDoubleDif(m)))
      case (_, GIntDif(n), GDoubleDif(m)) => GDoubleDif(implemReal(f, toDoubleDif(n), m))
      
      case (_, n: GDif[_], GInt(m)) => binGroundOp(f, n, GIntDif(TDif.constant(m)))
      case (_, n: GDif[_], GDouble(m)) => binGroundOp(f, n, GDoubleDif(TDif.constant(m)))
      case (_, GInt(n), m: GDif[_]) => binGroundOp(f, GIntDif(TDif.constant(n)), m)
      case (_, GDouble(n), m: GDif[_]) => binGroundOp(f, GDoubleDif(TDif.constant(n)), m)
      
      case _  => GDouble(implem2(f, extractDouble(vx), extractDouble(vy)))
    }
  }
  def matrixMultiply[A](u:List[Value[_]], v:List[Value[_]]): Value[A] = {
    val au = transMatrixArray(u)
    val av = transMatrixArray(v)
    val aun = au.length;    val avn = av.length;
    val aum = au(0).length; val avm = av(0).length;
    if(aum != avn)
      error("Matrix sizes are not suitable for multiplication")
    
    val result = for (row <- au)
	   yield for(col <- av.transpose)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_)
    
    val ls = result.map(x => x.toList).toList
    if(ls.length > 1)
      VVector(ls.map(x => VVector(x.map(y => VLit(GDouble(y))))))
    else
      VVector(ls(0).map(x => VLit(GDouble(x))))
  }
  
  def matrixPlusMinus[A](op:String,u:List[Value[_]], v:List[Value[_]]): Value[A] = {
    val au:Array[Array[Double]] = transMatrixArray(u)
    val av = transMatrixArray(v)
    val aun = au.length;    val avn = av.length;
    val aum = au(0).length; val avm = av(0).length;
    if(aum != avm || aun != avn )
      error("Matrice's size aren't suitable for plus or minus operation")   
    val result = 
      for ((row,col) <- au zip av)
        yield op match{
        case "+" =>  row zip col map Function.tupled(_+_) 
        case "-" =>  row zip col map Function.tupled(_-_) 
      }    
   val ls = result.map(x => x.toList).toList
    if(ls.length > 1)
      VVector(ls.map(x => VVector(x.map(y => VLit(GDouble(y))))))
    else
      VVector(ls(0).map(x => VLit(GDouble(x))))
  }
  def matrixScaleOp[A](op:String, au:Matrix, gv:Double): Matrix = {     
    for(row <- au)
 		yield op match{
 		  	case "+" => row map(_+gv)
 			case "*" => row map(_*gv)
	        case "/" => row map(_/gv)
	        case ".^" => row map(pow(_,gv))
	        case _ => throw UnknownOperator(op)
 			}
  }
  def isVector[A](u:List[Value[_]]):Boolean = {
    u.forall(x => x match{
      case VLit(_) => true
      case _ => false
    })
  }
  def transpose[A](matrix:Matrix):Matrix = {
    matrix.transpose
  }
  def matrixRow(matrix:Matrix):Int = matrix.length
  def matrixCol(matrix:Matrix):Int = matrix(0).length
  def changeSign(i:Int):Int = if(i%2 ==0) 1 else -1
  def printMatrix(matrix:Matrix) = {
    println("[")
    matrix.map(x => {print("[") ;x.map(y => print(y + ", ")) ; print("]");println("")})
    println("]")
  }
  
  // The input parameters for this method are the original matrix and the row and column 
  // index numbers that need to be deleted from the original matrix to create the sub-matrix
  def createSubMatrix(matrix:Matrix, excluding_row:Int, excluding_col:Int):Matrix = {
    val mat:Matrix = Array.ofDim(matrixRow(matrix) -1,matrixCol(matrix) -1)
    var r = -1
    for(i <- 0 to matrixRow(matrix) - 1; if i != excluding_row){
      r = r + 1
      var c = -1
      for(j <- 0 to matrixCol(matrix) - 1; if j != excluding_col){
        c = c + 1
        mat(r)(c) = matrix(i)(j)
      }
    }
    mat
  }
  def determinant(matrix:Array[Array[Double]]):Double = {
    if(matrix.length != matrix(0).length)
      error("Can't perform derminant operation because the size of matrix")
    var sum:Double = 0 
    var s:Double = 1
    if(matrix.length==1){  //bottom case of recursion. size 1 matrix determinant is itself.
      matrix(0)(0)
    }
    else{
      for(i <- 0 to matrix.length-1){ //finds determinant using row-by-row expansion
      var smaller:Array[Array[Double]]= Array.ofDim(matrix.length-1,matrix.length-1) //creates smaller matrix- values not in same row, column
      for(a <- 1 to matrix.length-1){
        for(b <- 0 to matrix.length-1){
          if(b<i){
            smaller(a-1)(b)=matrix(a)(b)
          }
          else if(b>i){
            smaller(a-1)(b-1)=matrix(a)(b)
          }
        }
      }
      s = changeSign(i)
      sum = sum + s*matrix(0)(i)*(determinant(smaller)) //recursive step: determinant of larger determined by smaller.
    }
    sum //returns determinant value. once stack is finished, returns final determinant.
   }
  }
  
  def cofactor(matrix:Matrix):Matrix = {
    val mat:Matrix = Array.ofDim(matrixRow(matrix),matrixCol(matrix))
    for(i <- 0 to matrixRow(matrix)-1)
      for(j <- 0 to matrixCol(matrix)-1)
        mat(i)(j) = changeSign(i)*changeSign(j)*determinant(createSubMatrix(matrix, i, j))
    mat
  }
  
  def inverse(matrix:Matrix):Matrix = {    
    matrixScaleOp("*", transpose(cofactor(matrix)), 1.0/determinant(matrix))
  }
  def binVectorOp[A](op:String, u:List[Value[_]], v:List[Value[_]]) : Value[A] = {
    lazy val du = extractDoubles(u)
    lazy val dv = extractDoubles(v)
    op match {
      case ".*"  => VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(d1*d2))))
      case "./"  => VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(d1/d2))))
      case ".^"  => VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(pow(d1,d2)))))
      case "+"   => 
        if(isMatrix(u) || isMatrix(v))
          matrixPlusMinus("+", u, v)
        else        
        VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(d1+d2))))
      case "-"   => 
         if(isMatrix(u) && isMatrix(v))
          matrixPlusMinus("-", u, v)
        else        
        VVector((du,dv).zipped map ((d1,d2) => VLit(GDouble(d1-d2))))
      case "dot" => VLit(GDouble(((du,dv).zipped map (_*_)).sum))
      case "*"   => 
        if(isMatrix(u) || isMatrix(v))
          matrixMultiply(u,v)
        else
        VLit(GDouble(((du,dv).zipped map (_*_)).sum))
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

  def unaryVectorOp[A](op:String, u:List[Value[A]]) : Value[A] = {
    lazy val du = extractDoubles(u)
    op match {
      case "length" => VLit(GInt(u.length))
      case "norm" => VLit(GDouble(math.sqrt((du map (d => d*d)).sum)))
      case "-" => binScalarVectorOp("*", GInt(-1), u)
      case "floor" => VVector(du map {d => VLit(GDouble(floor(d)))})
      case "ceil" => VVector(du map {d => VLit(GDouble(ceil(d)))})
      case "det" => VLit(GDouble(determinant(transMatrixArray(u))))
      case "inv" => 
        val ls = inverse(transMatrixArray(u)).map(x => x.toList).toList
        if(ls.length > 1)
          VVector(ls.map(x => VVector(x.map(y => VLit(GDouble(y))))))
        else
         VVector(ls(0).map(x => VLit(GDouble(x))))
      case "trans" =>      
          val ls = transpose(transMatrixArray(u)).map(x => x.toList).toList
          if(ls.length > 1)        
        	VVector(ls.map(x => VVector(x.map(y => VLit(GDouble(y))))))          
          else
        	VVector(ls(0).map(x => VLit(GDouble(x))))                  
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
    if(isMatrix(u)){
      val result = matrixScaleOp(op, transMatrixArray(u), dx)
      val ls = result.map(x => x.toList).toList
      VVector(ls.map(x => VVector(x.map(y => VLit(GDouble(y))))))
    }
    else  
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
  def evalOp[A](op:String, xs:List[Value[A]]) : Value[A] = {
    (op,xs) match {
       case ("==", x::y::Nil) => 
         VLit(GBool(x == y))
       case ("~=", x::y::Nil) => 
         VLit(GBool(x != y))
       case ("_:_:_", VLit(GInt(s))::VLit(GInt(d))::VLit(GInt(e))::Nil) =>
         sequenceOp(s,d,e)
       case ("format", VLit(GStr(s)) :: ps) => 
         VLit(GStr(s.format(ps.flatMap{
           case p @ VLit(_:GInt | _:GDouble) => extractDouble(p) :: Nil
           case VLit(GStr(x)) => x :: Nil
         }:_*))) 
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
       case ("trans", ls) => unaryVectorOp("trans",ls)
       case _ =>
         throw UnknownOperator(op)    
    }
  }

  /* eval Index(e, i) */
  def evalIndexOp[A](e: Value[A], i: List[Value[A]]) : Value[A] = {
    def lookup(i: Int, l: List[Value[A]]): Value[A] =
      try { l(i) } 
      catch { case _: IndexOutOfBoundsException => throw IndexOutOfBounds(i) }
    e match {
      case VVector(l) => i match {
        case VLit(GInt(idx)) :: Nil => lookup(idx, l)
        case VLit(gd: GDif[_]) :: Nil if gd.isValidInt => lookup(gd.toInt, l)
        case VVector(idxs) :: Nil => VVector(idxs.map(x => evalIndexOp(e,List(x))))
        case VVector(rows) :: VVector(columns) :: Nil => {
          val topRows = VVector(rows.map(x => evalIndexOp(e,List(x))))
          topRows match{
            case VVector(ls) => 
              VVector(ls.map(x => evalIndexOp(x,List(VVector(columns)))))
            case _ => throw IndexNoMatch(i)
          }
        }
        case VVector(rows) :: c ::Nil => evalIndexOp(e, VVector(rows) :: VVector(List(c)) :: Nil)
        case head :: tail => evalIndexOp(evalIndexOp(e,List(head)), tail)
      }    
      case _ => throw CantIndex() }
  }
  
  def valueToExpr(v:Value[_]):Expr = {
    v match{
      case VLit(n) => Lit(n)
      case VVector(ls) => ExprVector(ls map valueToExpr)
    }
  }

  val magicClassTxt =
    """model Simulator(time, timeStep, outputRows, continuousSkip, endTime, resultType, lastCreatedId)="""

  val deviceClassTxt =
  // ax, ay, az are respectively to the acceleration of each direction
  // alpha is the rotation of z-axis, beta is the rotation of x-axis, gamma is the rotation of y-axis
  // compass heading is the angle between device orientation and north of the earth
    """model Device(id) = initially ax=0, ay=0, az=0, alpha=0, beta=0, gamma=0, compassHeading=0"""

  lazy val magicClass = Parser.run(Parser.classDef, magicClassTxt)
  lazy val deviceClass = {
    val d = Parser.run(Parser.classDef, deviceClassTxt)
    ClassDef(d.name,d.fields,d.priv,d.priv.map{
      pr => Continuously(EquationT(Var(pr.x), Input(Dot(Var(self), Name("id",0)), pr.x.x)))
    })
  }

  def initStoreTxt(initStep: ResultType, timeStep: Double, outputRows: String, hypothesisReport: String, method: String) = 
    s"""#0.0 { className = Simulator, parent = %s, time = 0.0, timeStep = $timeStep, outputRows = "$outputRows", hypothesisReport = "$hypothesisReport", continuousSkip = 0,endTime = 10.0, resultType = @$initStep, nextChild = 0,method = "$method", orderOfIntegration = 4, seed1 = 0, seed2 = 0, variableCount = 0 }"""
  def initStoreInterpreter(initStep: ResultType = Initial, initTimeStep: Double = 0.015625, initOutputRows: String = "All", 
                       initHypothesisReport: String = "Comprehensive", initMethod: String = RungeKutta, isImperative: Boolean) =
      Parser.run(Parser.store, initStoreTxt(initStep, initTimeStep, initOutputRows, initHypothesisReport, initMethod).format( if (isImperative) "none" else "#0" ))

  // Register simulator parameters that should appear as completions in the code editor 
  // for any interpreter. Additional parameters are taken from Interpreter.parameters. 
  val visibleSimulatorFields = List("time", "timeStep", "endTime")

  def visibleParametersMap(initStore: CStore): Map[String,CValue] = {
    val initialMagic = initStore(magicId(initStore))
    visibleSimulatorFields.map(p => (p, initialMagic(name(p)))).toMap
  }
                                  
  // Register valid simulator parameters
  val simulatorFields = visibleSimulatorFields ::: List("outputRows", "hypothesisReport", "continuousSkip", "resultType", "lastCreatedId", "method", "variableCount")

  val specialFields = List("nextChild","parent","className","seed1","seed2")

  def threeDField(name: String) = 
    name == "_3D" || name == "_3DView"
      
  def patternVariable(name:String) = 
    name.split("__")(0) == "Pattern"
    
  def getExprVectorSize(e:Expr):List[Int] = e match{
    // All sub vectors should have the same size
    case ExprVector(ls) => ls match{
      case Nil => List(0)
      case _ => ls.size :: (getExprVectorSize(ls(0)))
    }
    case _ => Nil
  }
  /* Example: size:(2,2) -> (0,0), (0,1), (1,0), (1,1) */
  def getIndividualIndexesFromSize(size:List[Int]):List[List[Int]] = size match {
    case n :: Nil => (for(i <- 0 to n-1) yield List(i)).toList
    case n :: tail => 
      val res = getIndividualIndexesFromSize(tail)
      (0 to n-1).toList.map(i => for(j<-res) yield i::j).flatten
  }
  /* error checking */

  /**
   * Ensure that for each variable that has an ODE declared in the private section, there is
   * an equation in scope at the current time step. This is done by checking that for each
   * primed field name in each object in st, there is a corresponding CId-Name pair in odes.
   */
  def checkContinuousDynamicsAlwaysDefined(prog: Prog, odes: List[(ResolvedDot, List[Int])], st: CStore): Unit = {
    val declaredODENames = prog.defs.map(d => (d.name, (d.fields ++ d.priv.map(_.x)).filter(_.primes > 0))).toMap
    /* A map with tuple of name and size for vector prime variables*/
    val declaredODEVectors = prog.defs.map(d => (d.name, (d.priv.map(y => (y.x, y.rhs match {
      case ExprRhs(e) => getExprVectorSize(e)
      case _          => Nil
    })).filter(x => x._1.primes > 0 && x._2 != Nil)))).toMap
    st.foreach {
      case (o, _) =>
        if (o != magicId(st)) {
          declaredODENames.get(getCls(o, st)).map(_.foreach { n =>
            if (!odes.exists { case d => d._1.id == o && d._1.field.x == n.x })
              throw ContinuousDynamicsUndefined(o, n,None, Pretty.pprint(getObjectField(o, classf, st)), getTime(st))
          })

          declaredODEVectors.get(getCls(o, st)).map(_.foreach {
            case (n, size) =>
              val idxes = getIndividualIndexesFromSize(size)
              for (idx <- idxes)
                // There exists equation for specific index or for the whole vector
                if (!odes.exists { case d => d._1.id == o && d._1.field.x == n.x && (d._2 == idx || d._2 == Nil) })
                  throw ContinuousDynamicsUndefined(o, n,Some(idx), Pretty.pprint(getObjectField(o, classf, st)), getTime(st))
          })

        }
    }
  }

  /** Check for a duplicate assignment (of a specific kind) scheduled in assignments. */
  def checkDuplicateAssingments(assignments: List[(ResolvedDot,List[Int])], error: Name => DuplicateAssingment): Unit = {
    val duplicates = assignments.groupBy(a => (a._1.id, a._1.field,a._2)).filter{ case (_, l) => l.size > 1 }.toList
    if (duplicates.size != 0) {
      val first = duplicates(0)
      val x = first._1._2
      val poss = first._2.map{x => x._1.obj.pos}.sortWith{(a, b) => b < a}
      throw error(first._1._2).setPos(poss(0)).setOtherPos(poss(1))
    }
  }
  
   /** Old code for compatible with reference 2014 interpreter **/
   def checkDuplicateAssingments2014(assignments: List[ResolvedDot], error: Name => DuplicateAssingment): Unit = {
    val duplicates = assignments.groupBy(a => (a.id, a.field)).filter{ case (_, l) => l.size > 1 }.toList
    if (duplicates.size != 0) {
      val first = duplicates(0)
      val x = first._1._2
      val poss = first._2.map{_.obj.pos}.sortWith{(a, b) => b < a}
      throw error(first._1._2).setPos(poss(0)).setOtherPos(poss(1))
    }
  }
  def checkContinuousDynamicsAlwaysDefined2014(prog: Prog, odes: List[ResolvedDot], st: CStore): Unit = {
    val declaredODENames = prog.defs.map(d => (d.name, (d.fields ++ d.priv.map(_.x)).filter(_.primes > 0))).toMap
    st.foreach { case (o, _) =>
      if (o != magicId(st))
        declaredODENames.get(getCls(o, st)).map(_.foreach { n =>
          if (!odes.exists { case d => d.id == o && d.field.x == n.x })
            throw ContinuousDynamicsUndefined(o,n,None,Pretty.pprint(getObjectField(o, classf, st)), getTime(st))
        })
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
  
  def checkVariableDeclared(id: CId, n: Name, position: Position, st: CStore) : Unit =
    if (!deref(id,st).contains(n))
      throw new VariableNotDeclared(n).setPos(position)

  /* runtime analyses */
  
  /**
   * Updates the variableCount simulator field in st. This corresponds to 
   * the number of plots of model variables that will be shown, but not the 
   * number of columns in the table, as the latter also contains information 
   * such as class names etc. 
   */
  def countVariables(st: CStore): CStore = setObjectField(magicId(st), stateVars, VLit(GInt(countStateVars(st))), st)
  /**
   *  Computes the variableCount for a CStore.
   */
  def countStateVars(st: CStore): Int = {
      def objectVariables(o: CObject): Int = o.map{
      case (_, VObjId(_)) => 0 
      // _3D and _3DView objects should not increase variable count
      case (n,v) => if ((specialFields ++ List("_3D", "_3DView")) contains n.x) 0 else v.yieldsPlots getOrElse 0
    }.sum
    st.map{ case(id, o) => if (id == magicId(st)) 0 else objectVariables(o) }.sum
  }
  
  
  //
  // ODEs
  // 
  
  class Solver[Id, S <% RichStore[S,Id], R: Real](solverName: Value[_], val xs: S, val h: Double)(implicit f: Field[S,Id]) {
    private def msg(meth: String) = "Invalid integration method \"" + meth +
        "\". Please select one of: " + knownSolvers.mkString(", ")
    final def solve: S = {
      solverName match {
        case VLit(GStr(s))              => solveIfKnown(s) getOrElse (throw new Error(msg(s)))
        case VClassName(ClassName(c))   => throw new Error(msg(c))
        case m                          => throw new Error(msg(m.toString))
      }
    }
    def knownSolvers = List(EulerForward, RungeKutta, Taylor)
    lazy val VLit(GInt(taylorOrder)) = xs getInSimulator "orderOfIntegration"
    def solveIfKnown(name: String) : Option[S] = name match {
      case EulerForward => Some(solveIVPEulerForward(xs, h))
      case RungeKutta   => Some(solveIVPRungeKutta(xs, h))
      case Taylor       => Some(solveIVPTaylor[Id,S,R](xs, h, taylorOrder))
    }
  }

  def solveIVPEulerForward[Id, S <% RichStore[S,Id],V](xs: S, h: Double)(implicit f: Field[S,Id]): S =
    xs +++ f(xs) *** h

  def solveIVPRungeKutta[Id, S <% RichStore[S,Id],V](xs: S, h: Double)(implicit f: Field[S,Id]): S = {
    val k1 = f(xs)
    val k2 = f(xs +++ k1 *** (h/2)) 
    val k3 = f(xs +++ k2 *** (h/2))
    val k4 = f(xs +++ k3 *** h)
    xs +++ (k1 +++ k2 *** 2 +++ k3 *** 2 +++ k4) *** (h/6)
  }
  
  def solveIVPTaylor[Id, S <% RichStore[S,Id], R: Real](s: S, h: Double, orderOfIntegration: Int)(implicit f: Field[S,Id]): S = {
    require (orderOfIntegration > 0, s"Order of integration ($orderOfIntegration) must be greater than 0")
    val ode = f map (TAD lift _)
    val rIsReal = implicitly[Real[R]]
    val hl = rIsReal fromDouble h
    // compute Taylor coefficients of order 0 to orderOfIntegration
    val taylorCoeffs = (1 to orderOfIntegration).foldLeft(TAD lift s) {
      case (sTmp, i) => // sTmp contains coeffs up to order i-1
        val fieldApplied = TAD lift ode(sTmp) // FIXME Eliminate unnecessary lifting
        // compute the i-th Taylor coefficients
        f.variables(s).foldLeft(sTmp) { // we are modifying from the store containing the coefficients up to order i-1
          case (sUpdTmp, (id, n)) =>
            sUpdTmp updated (id, n, // update i:th Taylor coeff for variable (id, n)
              mapValuePair(sTmp(id, n), fieldApplied(id, n), { (gdif: GDif[R], der: GDif[R]) =>
                val coeff = gdif.dif.coeff updated (i, der.dif.coeff(i - 1) / (rIsReal fromInt i))
                VLit(gdif updated TDif(coeff, i + 1)) })) } // FIXME How many?
    }
    // the Taylor series // FIXME Does it not make more sense to accumulate solution when computing taylorCoeffs?
    val solution = f.variables(s).foldLeft(taylorCoeffs) { case (sTmp, (id, n)) => 
      sTmp updated (id, n, // sum the Taylor coeffs from the store in which they were computed (paranoia)
        mapValue(taylorCoeffs(id, n), { gdif: GDif[R] =>
          val vNext = gdif.dif.coeff.zipWithIndex.map { case (x, i) =>
            if (i <= orderOfIntegration) x * rIsReal.pow(hl, rIsReal fromInt i) // FIXME Use powOnInt 
            else rIsReal.zero }.take(orderOfIntegration).reduce(_ + _)
          VLit(rIsReal groundValue vNext) }))
    }
    TAD lower solution
  }

  /** Representation of a set of ODEs. 
   *  FieldId is a type, specific to the store type S, whose values 
   *  can be used to uniquely identify values in the store. */
  abstract class Field[S /* store */, Id /* object id */] {
    /** Evaluate the field (the RHS of each equation in ODEs) in s. */
    def apply(s: S): S
    /** Returns the set of variables affected by the field.
     * These are the LHSs of each ODE and the corresponding unprimed variables. */
    def variables(s: S): List[(Id, Name)]
    /** Map m over the RHS of ODE */
    def map(m: Expr => Expr): Field[S,Id]    
  }

  /** Embedded DSL for expressing integrators. */
  abstract class RichStore[S /* store */, Id /* object id */] {
    def +++(that: S): S
    def ***(that: Double): S
    def map(m: Value[Id] => Value[Id]): S
    def apply(id: Id, n: Name): Value[Id]
    def updated(id: Id, n: Name, v: Value[Id]): S
    def getInSimulator(variable: String): Value[Id]
  }

  /** Compute hypothesis outcomes (for non-rigorous intepreters) */
  def computeHypothesisOutcomes(outcome: Value[Any], time: => Double, resultType: ResultType, counterEx: => Set[(Dot, Value[Any])]) =
    (outcome, resultType) match {
      /* Use TestSuccess as default as it is the unit of HypothesisOutcome.pick */
      case (VLit(GBool(true)), _)                                   => (Some(TestSuccess), Some(TestSuccess), TestSuccess)
      case (VLit(GBool(false)), Initial)                            => (Some(InitialTestFailure(counterEx)), Some(TestSuccess), TestSuccess)
      case (VLit(GBool(false)), Discrete | Continuous | FixedPoint) => (Some(TestSuccess), Some(TestSuccess), TestFailure(time, counterEx))
    }
  
}
