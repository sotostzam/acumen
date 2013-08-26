package acumen
package interpreters
package compiler

import scala.collection.immutable.HashMap

import acumen.Errors._
import acumen.Pretty._
import acumen.util.Conversions._
import acumen.util.Random
import acumen.interpreters.Common.{ classDef }
import acumen.util.Canonical.{
  childrenOf, 
  classf,
  cmain,
  cmagic,
  endTime,
  nextChild,
  parentOf,
  parent,
  seedOf,
  seed1,
  seed2,
  self,
  time,
  timeStep
}

// Notes on assumptions:
//   Assuming vector length can be determined at compile time

// Note on parentheses handling:  If a function returns an expression
// that might need parantances when combined with other expression it
// should go ahead and wrap the the string in parentheses.

case class CType (name: String, arrayPart: String = "")

object CType {
  def ptr(name: String) = CType(name + " *")
  def mutRef(name: String) = CType(name + " &")
  def cref(name: String) = CType("const " + name + " &")
}

case class CVar (name: String, ctype: CType) {
  def toDecl = ctype.name + " " + name + ctype.arrayPart
}

object Collector {
  val structDefns = new scala.collection.mutable.ListBuffer[String]
  val funDecls = new scala.collection.mutable.ListBuffer[String]
  val funDefns = new scala.collection.mutable.ListBuffer[String]
  val symbols = new scala.collection.mutable.HashSet[String]

  def newStruct(name: String, fields: List[CVar]) {
    val cr = new CompileWriter
    cr.print("typedef struct " + name + " {").newline.indent(2)
    fields.foreach { el => 
      cr.print(el.toDecl + ";").newline
    }
    cr.indent(-2).print("} " + name + ";").newline
    structDefns += cr.toString
    symbols.add(name)
  }
  def newFun(name: String, args: List[CVar], returnType: String, body: String, flags: String = "static ") {
    val cr = new CompileWriter
    cr.print(flags + returnType + " " + name + "(");
    cr.print(args map {el => el.toDecl} mkString(", "));
    cr.print(") {").newline
    cr.print(body)
    cr.print("}").newline
    funDefns += cr.toString
    symbols.add(name)
  }
  def addClass(cc: CompiledClass) {
    structDefns += ("class " + cc.name 
                    + (if (cc.inherits != null) " : public " + cc.inherits else "") 
                    + " {\npublic:\n" + cc.body.toString + "};\n")
    symbols.add(cc.name)
  }
  def haveSym(name: String) = symbols.contains(name)
  def writeOut = {
    val out = new java.io.PrintWriter(new java.io.FileWriter("model.cpp"))
    def section(desc: String) = {
      out.print("\n");
      out.print("//////////////////////////////////////////////////////\n")
      out.print("//\n");
      out.print("// " + desc + "\n");
      out.print("//\n\n");
    }
    section("prelude.hpp");
    scala.io.Source.fromInputStream(getClass().getResourceAsStream("prelude.hpp")).getLines()
      .foreach {v => out.print(v); out.print("\n")}
    section("class/struct definitions")
    structDefns.foreach {v => out.print(v); out.print("\n")}
    section("class/struct definitions")
    funDecls.foreach {v => out.print(v); out.print("\n")}
    section("method/function definations")
    funDefns.foreach {v => out.print(v); out.print("\n")}
    section("acumen.cpp")
    scala.io.Source.fromInputStream(getClass().getResourceAsStream("acumen.cpp")).getLines()
      .foreach {v => out.print(v); out.print("\n")}
    System.out.println("*** C++ Code Written to model.cpp")
    out.close
  }
}

class CompiledClass(val name: String, val inherits : String = null) {
  val body = new CompileWriter(2)
  def addField(v: CVar) = {body.print(v.toDecl).print(";").newline}
  def addMethod(methodName: String, args: List[CVar], returnType: String, methodBody: String, flags: String = "") = {
    val argsStr = "(" + args.map{el => el.toDecl}.mkString(", ") + ")";
    body.print(flags + returnType + " " + methodName + argsStr + ";").newline
    val defn = new CompileWriter;
    defn.print(returnType + " " + name + "::" +  methodName + argsStr + " {").newline
    defn.printMultiline(methodBody);
    defn.print("}").newline
    Collector.funDefns += defn.toString
  }
  def addConstructor(args: List[CVar], initList: String, methodBody: String) = {
    val argsStr = "(" + args.map{el => el.toDecl}.mkString(", ") + ")";
    body.print(name + argsStr + ";").newline
    val defn = new CompileWriter;
    defn.print(name + "::" +  name + argsStr).newline
    if (initList != null)
      defn.indent(2).print(initList).newline.indent(-2)
    defn.print("{").newline
    defn.printMultiline(methodBody);
    defn.print("}").newline
    Collector.funDefns += defn.toString
  }
}

class CompileWriter(initial_indent : Int = 0) {
  var indent_amount = initial_indent
  var need_indent = true;
  var buf = new scala.collection.mutable.StringBuilder;

  def print(str: String) = {
    if (need_indent) {buf ++= " "*indent_amount; need_indent = false;}
    buf ++= str; this;}
  def printMultiline(str:String) {
    need_indent = true;
    buf ++= str; 
    this; 
  }
  def newline = {buf += '\n'; need_indent = true; this;}
  def indent(v: Int) = {indent_amount += v; this;}
  override def toString : String = {buf.toString}
}

class Interpreter extends acumen.CStoreInterpreter {
  val I = Interpreter
  import acumen._

  def id = null
  
  type Store = I.Store
  def init(prog: Prog) = null
  def fromCStore(st: CStore, root: CId) = null
  def repr(st: Store) = null

  def step(p: Prog, st: Store) = null
}

object Interpreter {
  val FIXME = null
  val UNSUPPORTED = FIXME // these should throw a specific exception

  type Store = Object
  type ObjId = Object
  type Val = Value[ObjId]
  type Env = scala.collection.mutable.Map[Name, TypeLike]
  
  type MMap[A, B] = scala.collection.mutable.Map[A, B]
  val MMap = scala.collection.mutable.Map

  def magicClassTxt =
    """class Simulator(time, timeStep, endTime, resultType) end"""

  lazy val magicClass = Parser.run(Parser.classDef, magicClassTxt)

  def compile(prog: Prog, typeChecker: TypeCheck) = {

    magicClass._types = typeChecker.simulator._types
    // ^^ get type information into magicClass (i.e. simulator, evil I know)

    compileClass(magicClass, prog, magicClass)
    compileClass(classDef(cmain, prog), prog, magicClass)
    
    val body = new CompileWriter(2)

    Collector.writeOut

    //gnuPlot(magicObj, mainObj)
  }

  // FIXME: Fix or remove
  // def gnuPlot(magicObj: Object, mainObj: Object) {
  //   val out = new java.io.PrintWriter(new java.io.FileWriter("model.gpl"))
  //   out.println("set terminal png")
  //   out.println("set ylabel \"Time\"")
  //   val magicFields = getFilteredSortedFields(magicObj)
  //   val mainFields = getFilteredSortedFields(mainObj)
  //   val timeIdx = mainFields.size + magicFields.indexWhere { case (n,_,_,_) => n.x == "time" } + 1
  //   var idx = 1
  //   mainFields.foreach{ case (name, arrayIdx, f, _) =>
  //     if (f == "%f") {
  //       out.println()
  //       out.println("set xlabel \"" + name.x + "'" * name.primes + arrayPart(arrayIdx) +"\"")
  //       val arrayPartForFn = arrayIdx match { case Some(i) => "_" + i; case _ => ""}
  //       out.println("set output \"model_" + to_c_name(name) + arrayPartForFn + ".png\"")
  //       out.println("plot \"model.res\" using " + timeIdx + ":" + idx + " with lines notitle")
  //     }
  //     idx += 1            
  //   }
  //   out.close
  // }

  def compileClass(cd: ClassDef, p: Prog, magic: ClassDef) : Unit = {
    val cn =  cd.name.x;
    Collector.symbols.add(cn);

    val cc = new CompiledClass(cn, "AcumenObject");
    
    // Create a c struct for the objec
    getAllFields(cd).foreach { case (name) => 
      cc.addField(CVar(to_c_name(name), to_c_type(cd._types(name))))
    }

    // Output the initialization function
    val args = cd.fields.map { name => CVar(to_c_name(name), to_c_type(cd._types(name))) }
    var body = new CompileWriter(4)
    cd.fields.foreach { name => 
      body.print("this->" + to_c_name(name) + " = " + to_c_name(name) + ";").newline
    }
    cd.priv.foreach { case Init (name, rhs) => 
      body.print("this->" + to_c_name(name) + " = ")
      rhs match {
        case NewRhs(c, fields) => body.print(compileCreate(c, fields, cd._types, p, magic))
        case ExprRhs(e) => body.print(compileExpr(e, p, cd._types))
      }
      body.print(";").newline
    }
    cc.addConstructor(List(CVar("p", CType.ptr("AcumenObject"))) ++ args, 
                      ": AcumenObject(" + to_c_string(cn) + ", p)",
                      body.toString)

    // Output a function for the object actions
    body = new CompileWriter(4)
    if (cn != "Simulator")
      compileActions(body, cd.body, cd._types, p, magic)
    body.print("return KEEP_ME;").newline
    cc.addMethod("step", List(CVar("resultType", CType("ResultType")), 
                              CVar("somethingChanged", CType.mutRef("bool")), 
                              CVar("timeStep", CType("double"))), 
                 "KillMe", body.toString)
    cc.addMethod("discrete_step", List(CVar("somethingChanged", CType.mutRef("bool"))),
                 "KillMe", "    return step(DISCRETE, somethingChanged, 0.0);\n");
    cc.addMethod("continuous_step", List(CVar("timeStep", CType("double"))),
                 "void", "    bool dummy = false; step(CONTINUOUS, dummy, timeStep);\n")
    val body2 = new CompileWriter(4)
    compileDumpObjHeader(body2, "", cd, p)
    cc.addMethod("dump_header", List(), "void", body2.toString)
    val body3 = new CompileWriter(4)
    compileDumpObj(body3, "", cd, p)
    cc.addMethod("dump_state_line", List(), "void", body3.toString)
    val body4 = new CompileWriter(4)
    compileDumpObjState(body4, "", cd, p)
    cc.addMethod("dump_state", List(), "void", body4.toString)
    Collector.addClass(cc)
  }

  def compileActions(cr: CompileWriter, as: List[Action], env: Env, p: Prog, magic: ClassDef) =
    as.foreach {a => compileAction(cr, a, env, p, magic)}

  def compileAction(cr: CompileWriter, a: Action, env: Env, p: Prog, magic: ClassDef) {
    a match {
      case IfThenElse(c, a1, a2) =>
        cr.print("if (")
        cr.print(compileExpr(c, p, env))
        cr.print(") {").newline.indent(2)
        compileActions(cr, a1, env, p, magic)
        cr.indent(-2).print("} else {").newline.indent(2)
        compileActions(cr, a2, env, p, magic)
        cr.indent(-2).print("}").newline
      case ForEach(i, l, b) =>
        cr.print("/* UNIMPLEMENTED: ForEach */").newline
      case Switch(s, cls) =>
        cr.print("{").newline.indent(2)
        cr.print(CVar("switchVal", to_c_type(s._type)).toDecl + " = ")
        cr.print(compileExpr(s, p, env))
        cr.print(";").newline
        var first = true
        cls.foreach { cl => 
          if (!first)
            cr.indent(-2).print("} else ")
          cr.print("if (" + c_cmp(s._type, "switchVal", to_c_value(cl.lhs))  + ") {").newline.indent(2)
          compileActions(cr, cl.rhs, env, p, magic)
          first = false
        }
        cr.indent(-2).print("} else { abort(); }").newline
        cr.indent(-2).print("}").newline
      case Discretely(da) =>
        cr.print("if (resultType == DISCRETE) {").newline.indent(2)
        compileDiscreteAction(cr, da, env, p, magic)
        cr.indent(-2).print("}").newline
      case Continuously(ca) =>
        cr.print("if (resultType == CONTINUOUS) {").newline.indent(2)
        compileContinuousAction(cr, ca, env, p, magic)
        cr.indent(-2).print("}").newline
    }
  }

  def compileDiscreteAction(cr: CompileWriter, a: DiscreteAction, env: Env, p: Prog, magic: ClassDef) =
    a match {
      case Assign(ed@Dot(e, x), t) =>
        val lhs = compileExpr(ed, p, env) // FIXME: Verify this is doing the right thing
        val rhs = compileExpr(t, p, env)
        ed._type.vectorSize match {
          case -1 => 
            cr.print("if (" + c_cmp_inv(ed._type, lhs,rhs) + " ) {").newline.indent(2)
            cr.print(lhs + " = " + rhs + ";").newline
            cr.print("somethingChanged = 1;").newline
            cr.indent(-2).print("}").newline
          case sz => 
            cr.print(mkCallVectorAssignIfChanged(sz, lhs, rhs, p, env) + ";").newline
        }
      case Create(lhs, c, es) =>
        val createExpr = compileCreate(c, es, env, p, magic)
        cr.print(createExpr + ";").newline
        cr.print("somethingChanged = 1;").newline
        //lhs match {
        //  case None => logModified
        //  case Some(Dot(e, x)) =>
        //    val VObjId(Some(id)) = evalExpr(e, p, env)
        //    logModified || setField(id, x, VObjId(Some(fa)))
        //  case Some(_) => throw BadLhs()
        //}
      case Elim(Var(Name("self", 0))) =>
        cr.print("return KILL_ME;").newline
      case _ => 
        cr.print("/*Unimplemented*/").newline
    }

  def compileContinuousAction(cr: CompileWriter, a: ContinuousAction, env: Env, p: Prog, magic: ClassDef) : Unit =
    a match {
      case EquationT(Dot(e, x), t) =>
        // FIXME: Assuming single object case
        cr.print(to_c_name(x) + " = ");
        cr.print(compileExpr(t, p, env))
        cr.print(";").newline
      case EquationI(Dot(e, x), t) =>
        val lhs = env(x)
        // FIXME: Assuming single object case
        lhs.vectorSize match {
          case -1 =>
            cr.print(to_c_name(x) + " += ")
            cr.print(compileExpr(t, p, env))
            cr.print(" * " + "timeStep")
          case sz =>
            cr.print(mkCallVectorContinuous(sz, to_c_name(x), compileExpr(t, p, env), p, env) + ";").newline
        }
        cr.print(";").newline
      case _ =>
        throw ShouldNeverHappen() // FIXME: fix that with refinement types
    }

  def unimplemented(e: Expr) : String = {
    "/* UNIMPLEMENTED: " + e + "*/"
  }

  def vectorType(sz: Int) : String = {
    val typeName = "Vec" + sz
    if (!Collector.haveSym(typeName)) {
      val cc = new CompiledClass(typeName);
      cc.addField(CVar("d", CType("double", arrayPart(sz))))
      cc.body.print("double operator[] (int i) const {return d[i];};").newline
      cc.body.print("double & operator[] (int i) {return d[i];};").newline
      cc.body.print(typeName + "() {}").newline
      val args = Seq.range(0,sz).map{i => "i" + i}
      cc.body.print(typeName + "(" + Seq.range(0,sz).map{i=>"double i" + i}.mkString(", ") + ") {").newline.indent(2);
      Seq.range(0,sz).foreach{i => cc.body.print("d[" + i + "] = i" + i + ";").newline}
      cc.body.indent(-2).print("}").newline
      Collector.addClass(cc)
    }
    typeName
  }

  def compileCreate(name: ClassName, args: List[Expr], env: Env, p: Prog, magic: ClassDef) : String = {
    if (!Collector.haveSym(name.x)) {
      val cd = classDef(name, p)
      compileClass(cd, p, magic)
    }
    "new " + name.x + "(this, " + args.map{e => compileExpr(e,p,env)}.mkString(", ") + ")"
  }

  def compileExpr(e: Expr, p: Prog, env: Env): String = {
    e match {
      case Lit(i)        => to_c_value(i)
      case ExprVector(l) => 
        vectorType(l.size) + "(" + l.map{e => compileExpr(e,p,env)}.mkString(", ") + ")"
      case Var(n)        => to_c_name(n)
      case Dot(v, Name("children", 0)) => unimplemented(e)
        //val VObjId(Some(id)) = eval(env, v)
        ///id synchronized { VList((id.children map VObjId[ObjId]).toList) }
        //VList((id.children map (c => VObjId(Some(c)))).toList)
      /* e.f */
      case Dot(Var(Name("self", 0)), f) => "this->" + to_c_name(f)
      case Dot(e, f) => 
        compileExpr(e, p, env) + "->" + to_c_name(f)
        //val VObjId(Some(id)) = eval(env, e)
        //cr.print("obj->" + e + "->" + f) // FIXME NOW (Once I find a test case)
      /* x && y */
      case Op(Name("&&", 0), x :: y :: Nil) => compileToCBinOp("&&", x, y, p, env)
      case Op(Name("||", 0), x :: y :: Nil) => compileToCBinOp("||", x, y, p, env)
      case Op(Name(op, 0), args) => compileOp(e, op, args,p,env)
      /* sum e for i in c st t */
      case Sum(e, i, c, t) => unimplemented(e)
      case TypeOf(cn) => unimplemented(e)
    }
    //cr.print("/*="+ e + "*/ ") 
  }

  def compileOp(exp: Expr, op:String, args: List[Expr], p: Prog, env: Env) : String = {
    (op,args) match {
       case ("==", x :: y :: Nil) => compileToCBinOp("==", x, y, p, env)
       case ("~=", x :: y :: Nil) => compileToCBinOp("!=", x, y, p, env)
       case ("_:_:_", _) =>
         unimplemented(exp)
       case (_, x :: Nil) if x._type.numericLike =>
         compileUnaryOp(op,x,p,env)
       case (_, x :: y :: Nil) if x._type.numericLike && y._type.numericLike =>  
         compileBinOp(op,x,y,p,env) 
       case (_, x :: Nil) if x._type.isVector =>
         compileUnaryVectorOp(op,x,p,env)
       case (_, x :: y :: Nil) if x._type.isVector && y._type.isVector =>
         compileBinVectorOp(op,x,y,p,env)
       case (_, x :: y :: Nil) if x._type.numericLike && y._type.isVector =>
         compileBinScalarVectorOp(op,x,y,p,env)
       case (_, x :: y :: Nil) if x._type.isVector && y._type.numericLike =>
         compileBinVectorScalarOp(op,x,y,p,env)
       //case (_, VList(u)::Nil, _) =>
       //  unimplemented(exp)
       case _ =>
         throw UnknownOperator(op)    
    }
  }

  /* purely functional unary operator evaluation 
   * at the ground values level */
  def compileUnaryOp(f:String, xe: Expr, p: Prog, env: Env) : String = {
    def implem(f:String) = f match {
        case "sin" => compileToCFunCall("sin", List(xe), p, env)
        case "cos" => compileToCFunCall("cos", List(xe), p, env)
        case "tan" => compileToCFunCall("tan", List(xe), p, env)
        case "acos"=> compileToCFunCall("acos", List(xe), p, env)
        case "asin"=> compileToCFunCall("asin", List(xe), p, env)
        case "atan"=> compileToCFunCall("atan", List(xe), p, env)
        case "toRadians"  => compileToCFunCall("???", List(xe), p, env)
        case "toDegrees"  => compileToCFunCall("???", List(xe), p, env)
        case "exp"  => compileToCFunCall("exp", List(xe), p, env)
        case "log"  => compileToCFunCall("log", List(xe), p, env)
        case "log10"  => compileToCFunCall("log10", List(xe), p, env)
        case "sqrt" => compileToCFunCall("sqrt", List(xe), p, env)
        case "cbrt" => compileToCFunCall("cbrt", List(xe), p, env)
        case "ceil"=> compileToCFunCall("ceil", List(xe), p, env)
        case "floor"=> compileToCFunCall("floor", List(xe), p, env)
        case "rint"=> compileToCFunCall("rint", List(xe), p, env)
        case "round"=> compileToCFunCall("roundf", List(xe), p, env)
        case "sinh" => compileToCFunCall("sinh", List(xe), p, env)
        case "cosh" => compileToCFunCall("cosh", List(xe), p, env)
        case "tanh" => compileToCFunCall("tanh", List(xe), p, env)
        case "signum"=> compileToCFunCall("???", List(xe), p, env)

// Should abs and and - above?

    }
    (f, xe._type) match {
      case ("not", BoolType)   => "(!" + compileExpr(xe, p, env) + ")"
      case ("abs", IntType)    => compileToCFunCall("abs", List(xe), p, env)
      case ("-",   IntType)    => "(-" + compileExpr(xe, p, env) + ")"
      case ("abs", NumericType) => compileToCFunCall("fabs", List(xe), p, env)
      case ("-",   NumericType) => "(-" + compileExpr(xe, p, env) + ")"
      case ("round", NumericType) => compileToCFunCall("round", List(xe), p, env)
      case _                   => implem(f)
    }
  }

  /* purely functional binary operator evaluation 
   * at the ground values level */
  def compileBinOp(f:String, xe: Expr, ye: Expr, p: Prog, env: Env) = {
    def implem1(f:String) = f match {
      // Int bin ops
      case "+" => compileToCBinOpForceInt("+", xe, ye, p, env)
      case "-" => compileToCBinOpForceInt("-", xe, ye, p, env)
      case "*" => compileToCBinOpForceInt("*", xe, ye, p, env)
      case "<<" => compileToCBinOpForceInt("<<", xe, ye, p, env)
      case ">>" => compileToCBinOpForceInt(">>", xe, ye, p, env)
      case "&"  => compileToCBinOpForceInt("&", xe, ye, p, env)
      case "|"  => compileToCBinOpForceInt("|", xe, ye, p, env)
      case "%"  => compileToCBinOpForceInt("%", xe, ye, p, env)
      case "xor" => compileToCBinOpForceInt("^", xe, ye, p, env)
    }
    def implem2(f:String) = f match {
      // Double bin ops
      case "+" => compileToCBinOp("+", xe, ye, p, env)
      case "-" => compileToCBinOp("-", xe, ye, p, env)
      case "*" => compileToCBinOp("*", xe, ye, p, env)
      case "^" => compileToCFunCall("pow", List(xe, ye), p, env)
      case "/" => compileToCBinOp("/", xe, ye, p, env)
      case "atan2" => compileToCFunCall("atan2", List(xe, ye), p, env) 
      case _ => throw UnknownOperator(f)
    }
    def implem3(f:String) = f match {
      // Int bin ops
      case "<"  => compileToCBinOpForceInt("<", xe, ye, p, env)
      case ">"  => compileToCBinOpForceInt(">", xe, ye, p, env)
      case "<=" => compileToCBinOpForceInt("<=", xe, ye, p, env)
      case ">=" => compileToCBinOpForceInt(">=", xe, ye, p, env)
    }
    def implem4(f:String) = f match {
      // Double bin ops
      case "<" => compileToCBinOp("<", xe, ye, p, env)
      case ">" => compileToCBinOp(">", xe, ye, p, env)
      case "<=" => compileToCBinOp("<=", xe, ye, p, env)
      case ">=" => compileToCBinOp(">=", xe, ye, p, env)
    }
    (f, xe._type, ye._type) match {
      case (">="|"<="|"<"|">", IntType, IntType) => implem3(f)
      case ("<"|">"|"<="|">=", _, _) => implem4(f)
      case ("+"|"-"|"*"|"<<"|">>"|"&"|"|"|"%"|"xor", IntType, IntType) => implem1(f)
      case _  => implem2(f)
    }
  }

  def compileUnaryVectorOp(op: String,xe: Expr, p: Prog, env: Env) : String = {
    op match {
      //case "length" => VLit(GInt(u.length)) 
      case "norm" => mkCallVectorNorm(xe._type.vectorSize, xe, p, env)
      case _ => throw InvalidVectorOp(op)
    }
  }
  
  def compileBinVectorOp(op: String,
                         xe: Expr, ye: Expr, p: Prog, env: Env) : String = {
    val sz = xe._type.vectorSize
    def mkCall(fname: String, f: (String, String) => String) = {
      val fullName = "v_" + fname
      mkCallBinVectorFun(fullName, sz, CType.cref(vectorType(sz)), CType.cref(vectorType(sz)), xe, ye, f("x[i]", "y[i]"), p, env)
    }
    op match {
      case ".*"  => mkCall("times", (x,y) => x + "*" + y)
      case "./"  => mkCall("div", (x,y) => x + "/" + y)
      case ".^"  => mkCall("pow", (x,y) => "pow(" + x + "," + y + ")")
      case "+"   => mkCall("plus", (x,y) => x + "+" + y)
      case "-"   => mkCall("minus", (x,y) => x + "-" + y)
      case "dot" => mkCallVectorDot(sz, xe, ye, p, env)
      case "cross" => "/* unimplemented: cross */"
    }
  }

  def compileBinScalarVectorOp(op: String, 
                               xe: Expr, ye: Expr, p: Prog, env: Env) : String = {
    op match {
      case "+" => compileBinVectorScalarOp(op,xe,xe,p,env)
      case "*" => compileBinVectorScalarOp(op,ye,xe,p,env)
      case _ => throw InvalidScalarVectorOp(op)
    }
  }
  
  def compileBinVectorScalarOp(op: String,
                               xe: Expr, ye: Expr, p: Prog, env: Env) : String = {
    val sz = xe._type.vectorSize
    def mkCall(fname: String, f: (String, String) => String) = {
      val fullName = "vs_" + fname + "_" + sz
      mkCallBinVectorFun(fullName, sz, CType.cref(vectorType(sz)), CType("double"), xe, ye, f("x[i]", "y"), p, env)
    }
    op match {
      case "+" => mkCall("plus", (x,y) => x + "+" + y)
      case "*" => mkCall("times",(x,y) => x + "*" + y)
      case "/" => mkCall("div", (x,y) => x + "/" + y)
      case ".^" => mkCall("pow", (x,y) => "pow(" + x + "," + y + ")")
      case _ => throw InvalidVectorScalarOp(op)
    }
  }

  def mkCallBinVectorFun(fullName: String, sz: Int, 
                         xType: CType, yType: CType,
                         xe: Expr, ye: Expr, 
                         loopBodyExpr: String, p: Prog, env: Env) : String = {
    if (!Collector.haveSym(fullName)) {
      val body = new CompileWriter(2)
      body.print(vectorType(sz) +" res;").newline
      body.print("int i;").newline
      body.print("for (i = 0; i != " + sz + "; ++i) {").newline.indent(2)
      body.print("res[i] = " + loopBodyExpr + ";").newline
      body.indent(-2).print("}").newline
      body.print("return res;").newline
      Collector.newFun(fullName, List(CVar("x", xType), CVar("y", yType)),
                       vectorType(sz), body.toString, "static ")
    }
    return fullName + "(" + compileExpr(xe,p,env) + ", " + compileExpr(ye,p,env) + ")";
  }

  def mkCallVectorAssignIfChanged(sz: Int, xe: String, ye: String, p: Prog, env: Env) : String = {
    val fullName = "v_assign_if_changed";
    if (!Collector.haveSym(fullName)) {
      val body = new CompileWriter(2)
      body.print("int i = 0;").newline
      body.print("while (i != " + sz + ") {").newline.indent(2)
      body.print("if (x[i] != y[i]) {").newline.indent(2)
      body.print("x = y;").newline
      body.print("somethingChanged = 1;").newline
      body.print("return;").newline
      body.indent(-2).print("} else {").indent(2).newline
      body.print("++i;").newline
      body.indent(-2).print("}").newline
      body.indent(-2).print("}").newline
      Collector.newFun(fullName, List(CVar("x", CType.mutRef(vectorType(sz))), 
                                      CVar("y", CType.cref(vectorType(sz))),
                                      CVar("somethingChanged", CType.mutRef("bool"))),
                       "void", body.toString)
    }
    return fullName + "(" + xe + ", " + ye + ", somethingChanged)";
  }

  def mkCallVectorContinuous(sz: Int, xe: String, ye: String, p: Prog, env: Env) : String = {
    val fullName = "v_continuous";
    if (!Collector.haveSym(fullName)) {
      val body = new CompileWriter(2)
      body.print("int i;").newline
      body.print("for (i = 0; i != " + sz + "; ++i) {").newline.indent(2)
      body.print("x[i] += y[i] * timeStep;").newline
      body.indent(-2).print("}").newline
      Collector.newFun(fullName, List(CVar("x", CType.mutRef(vectorType(sz))), 
                                      CVar("y", CType.cref(vectorType(sz))),
                                      CVar("timeStep", CType("double"))),
                       "void", body.toString)
    }
    return fullName + "(" + xe + ", " + ye + ", timeStep)"
  }

  def mkCallVectorDot(sz: Int, xe: Expr, ye: Expr, p: Prog, env: Env) : String = {
    val fullName = "v_dot";
    if (!Collector.haveSym(fullName)) {
      val body = new CompileWriter(2)
      body.print("double res = 0;").newline
      body.print("int i;").newline
      body.print("for (i = 0; i != " + sz + "; ++i) {").newline.indent(2)
      body.print("res += x[i] * y[i];").newline
      body.indent(-2).print("}").newline
      body.print("return res;").newline
      Collector.newFun(fullName, List(CVar("x", CType.cref(vectorType(sz))), 
                                      CVar("y", CType.cref(vectorType(sz)))),
                       "double", body.toString)
    }
    return fullName + "(" + compileExpr(xe,p,env) + "," + compileExpr(ye,p,env) + ")";
  }

  def mkCallVectorNorm(sz: Int, xe: Expr, p: Prog, env: Env) : String = {
    val fullName = "v_norm";
    if (!Collector.haveSym(fullName)) {
      val body = new CompileWriter(2)
      body.print("double res = 0;").newline
      body.print("int i;").newline
      body.print("for (i = 0; i != " + sz + "; ++i) {").newline.indent(2)
      body.print("res += x[i] * x[i];").newline
      body.indent(-2).print("}").newline
      body.print("return sqrt(res);").newline
      Collector.newFun(fullName, List(CVar("x", CType.cref(vectorType(sz)))), 
                       "double", body.toString)
    }
    return fullName + "(" + compileExpr(xe,p,env) + ")";
  }

  //
  // Utility Function/Methods
  // 

  def to_c_name(name: Name) : String = {
    return name.x + "_" + name.primes
  }

  def to_c_name(name: Name, idx: Option[Int]) : String = {
    def cname = to_c_name(name)
    idx match {
      case Some(i) => cname + "[" + i + "]"
      case _ => cname
    }
  }

  def to_c_string(str: String) : String = "\"" + str + "\""

  def to_c_type(typ: TypeLike): CType = typ match {
    case IntType => CType("int")
    case NumericType => CType("double")
    case BoolType => CType("Boolean")
    case StrType => CType("const char *")
    case SeqType(st@FixedSize(lst)) if st.isNumeric =>  CType(vectorType(lst.size))
    case SeqType(_) => UNSUPPORTED
    case ResultTypeType => CType("ResultType")
    case ClassType(NamedClass(cn)) => CType(cn.x + " *")
    case ClassType(_) => UNSUPPORTED
    case v          => CType("/*"+v+"*/double") // FIXME
  }

  def to_c_value(value: GroundValue) = value match {
    case GInt(v) => v.toDouble.toString 
    // ^^ Hack forse ints to print as doubles for now
    // Avoided having to think about cases auch as "1/2" which in C would be 0 and not 0.5
    case GDouble(v) => v.toString
    case GBool(v) => v.toString
    case GStr(v) => to_c_string(v.toString)
    case v => "/*" + v.toString + "*/0.0" // FIXME
  }

  def to_c_value(value: Val) : String = {
    value match {
      case VLit(v) => to_c_value(v)
      case VClassName(ClassName(n)) => to_c_string(n)
      case VResultType(Discrete) => "DISCRETE"
      case VResultType(FixedPoint) => "???"
      case VResultType(Continuous) => "CONTINUOUS"
      case VVector(l) => vectorType(l.size) + "(" + l.map{v => to_c_value(v)}.mkString(", ") + ")"
      case _ => "NULL" // FIXME
    }
  }

  def c_cmp (theType: TypeLike, x: String, y: String) = theType match {
    case StrType => "(strcmp(" + x + ", " + y + ")" + "== 0)"
    case _ => x + " == " + y;
  }

  def c_cmp_inv (theType: TypeLike, x: String, y: String) = theType match {
    case StrType => "(strcmp(" + x + ", " + y + ")" + "!= 0)"
    case _ => x + " != " + y;
  }

  def arrayPart(i: Int) : String = {
    "[" + i + "]"
  }
  def arrayPart(idx: Option[Int]) : String = idx match {
    case Some(i) => "[" + i + "]"
    case _ => ""
  }

  def compileToCBinOp(cop: String, x: Expr, y: Expr, p: Prog, env: Env) : String = {
    "(" + compileExpr(x, p, env) + " " + cop + " " + compileExpr(y, p, env) + ")"
  }

  def compileToCBinOpForceInt(cop: String, x: Expr, y: Expr, p: Prog, env: Env) : String = {
    "(" + "(int)(" + compileExpr(x, p, env) + ") " + cop + " (int)(" + compileExpr(y, p, env) + "))"
  }

  def compileToCFunCall(funName: String, args: List[Expr], p: Prog, env: Env) : String = {
    funName + "(" + args.map{arg => compileExpr(arg, p, env)}.mkString(", ") + ")"
  }

  def compileDumpObjHeader(cr: CompileWriter, prefix: String, cd: ClassDef, p: Prog) : Unit = {
    val cn =  cd.name.x
    val fields = getFilteredSortedFields(cd);
    cr.print("printf(\"%s\", \"");
    cr.print(fields.map{case (n,i,_,_) => cn + "." + n.x + "'"*n.primes+arrayPart(i)}.mkString(" "))
    cr.print(" \");").newline
  }

  def compileDumpObj(cr: CompileWriter, prefix:String, cd: ClassDef, p: Prog) : Unit = {
    val fields = getFilteredSortedFields(cd);
    cr.print("printf(\"");
    cr.print(fields.map( {case (_,_,f,_) => f}).mkString(" "))
    cr.print(" \", ")
    cr.print(fields.map{case (n,i,_,f) => f(prefix + to_c_name(n,i))}.mkString(","))
    cr.print(");").newline
  }

  def compileDumpObjState(cr: CompileWriter, prefix:String, cd: ClassDef, p: Prog) : Unit = {
    val cn =  cd.name.x;
    val fields = getFilteredSortedFields(cd);
    cr.print("printf(\"%s \" " + to_c_string(cn) + "\" {\\n");
    cr.print(fields.map( {case (n,i,fmt,_) => "  " + n.x + "'"*n.primes+arrayPart(i) + " = " + fmt + "\\n"}).mkString(""))
    cr.print("}\\n\", id.to_str(), ")
    cr.print(fields.map{case (n,i,_,f) => f(prefix + to_c_name(n,i))}.mkString(","))
    cr.print(");").newline
  }

  def getAllFields(cd: ClassDef) : List[Name] = 
    cd.fields ++ cd.priv.map {case Init(name, _) => name}

  def getFilteredSortedFields(cd: ClassDef) : List[(Name,Option[Int],String,String => String)] = {
    val res = getAllFields(cd).sortWith{(x,y) => x < y}.map { name =>
      cd._types(name) match {
        case NumericType => List((name, None, "%f", {n:String => n}))
        case IntType => List((name, None, "%i", {n:String => n}))
        case StrType => List((name, None, "%s", {n:String => n}))
        case ResultTypeType => List((name, None, "%s", // FIXME: Add support for @FixedPoint
                                     {n:String => n + "== DISCRETE ? \"@Discrete\" : \"@Continuous\""}))
        case SeqType(st@FixedSize(lst)) if st.isNumeric =>
          (0 until lst.size).map{i => (name, Some(i), "%f", {n:String => n})}.toList 
        case _ => Nil
      }
    }
    List.flatten(res)
  }

}
