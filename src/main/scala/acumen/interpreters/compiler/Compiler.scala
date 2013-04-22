package acumen
package interpreters
package compiler

import scala.collection.immutable.HashMap

import acumen.Errors._
import acumen.Pretty._
import acumen.util.Conversions._
import acumen.util.Random
import acumen.interpreters.Common.{ classDef, evalOp }
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
  stepType,
  time,
  timeStep
}

// Notes on assumptions:
//   Assuming vector length can be determined at compile time

case class CType (name: String, arrayPart: String = "")

case class CVar (name: String, ctype: CType) {
  def toDecl = ctype.name + " " + name + ctype.arrayPart
}

object Collector {
  val prelude = new CompileWriter
  val structDefns = new scala.collection.mutable.ListBuffer[String]
  val globals = new CompileWriter
  val funDecls = new scala.collection.mutable.ListBuffer[String]
  val funDefns = new scala.collection.mutable.ListBuffer[String]
  val symbols = new scala.collection.mutable.HashSet[String]

  def newStruct(name: String, fields: List[CVar]) {
    val cr = new CompileWriter
    cr.print("typedef struct " + name + "{").newline.indent(2)
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
  def haveSym(name: String) = symbols.contains(name)
  def writeOut = {
    val out = new java.io.PrintWriter(new java.io.FileWriter("model.c"))
    out.print(prelude.toString)
    structDefns.foreach {v => out.print(v)}
    out.print(globals.toString)
    funDecls.foreach {v => out.print(v)}
    funDefns.foreach {v => out.print(v)}
    System.out.println("*** C Code Written to model.c")
    out.close
  }
}

class CompileWriter(initial_indent : Int = 0) {
  var indent_amount = initial_indent
  var need_indent = true;
  var buf = new scala.collection.mutable.StringBuilder;

  def print(str: String) = {
    if (need_indent) {buf ++= " "*indent_amount; need_indent = false;}
    buf ++= str; this;}
  def newline = {buf += '\n'; need_indent = true; this;}
  def indent(v: Int) = {indent_amount += v; this;}
  override def toString : String = {buf.toString}
}

class Interpreter extends acumen.CStoreInterpreter {
  val I = Interpreter
  import acumen._
  
  type Store = I.Store
  def init(prog: Prog) = I.init(prog)
  def fromCStore(st: CStore, root: CId) = null
  def repr(st: Store) = null

  def step(p: Prog, st: Store) = null
}

object Interpreter {
  import imperative.Common.{Object, evalExpr, mkObj, getClassOf, getField}

  type Store = Object
  type ObjId = Object
  type Val = Value[ObjId]
  type Env = Map[Name, Val]
  
  type MMap[A, B] = scala.collection.mutable.Map[A, B]
  val MMap = scala.collection.mutable.Map

  def magicClassTxt =
    """class Simulator(time, timeStep, endTime, stepType, lastCreatedId) end"""
  def initStoreTxt =
    """#0.0 { className = Simulator, parent = none, time = 0.0, timeStep = 0.01, 
              endTime = 10.0, stepType = @Discrete, nextChild = 0,
						  seed1 = 0, seed2 = 0 }"""

  lazy val magicClass = Parser.run(Parser.classDef, magicClassTxt)
  lazy val magicCObj = Parser.run(Parser.store, initStoreTxt)

  def init(prog: Prog): (Prog, Store) = {
    val magic = imperative.Common.fromCStore(magicCObj, CId(0))
    /* WARNING: the following line works because there is no children access check
       if one of the instructions of the provate section tries to access magic,
       and there was a check, this would crash (which we don't want) */
    val (sd1, sd2) = Random.split(Random.mkGen(0))
    val mainObj = mkObj(cmain, prog, None, sd1, List(VObjId(Some(magic))), magic, 1)
    //cr.print("int main() {").newline.indent(2)
    //cr.print("struct Main main_;").newline
    //cr.indent(-2).print("}").newline
    magic.seed = sd2
    //changeParent(magic, mainObj) //FIXME: Needed? (as  we are a compiler...)
    compile(magic,mainObj,prog)
    (Prog(magicClass :: prog.defs), mainObj)
  }

  def compile(magicObj: Object, mainObj: Object, prog: Prog) {

    Collector.prelude.print("#include <stdlib.h>").newline
    Collector.prelude.print("#include <math.h>").newline
    Collector.prelude.print("#include <stdio.h>").newline
    Collector.prelude.print("#include <string.h>").newline
    Collector.prelude.newline
    
    Collector.prelude.print("typedef enum {DISCRETE, CONTINUOUS} StepType;").newline
    Collector.prelude.print("typedef int Boolean;").newline

    Collector.globals.print("static Simulator simulator;").newline
    Collector.globals.print("static Boolean somethingChanged;").newline

    compileObj(magicObj, prog, magicObj)
    compileObj(mainObj, prog, magicObj)
    
    val body = new CompileWriter(2)
    body.print("init_Simulator(&simulator);").newline
    body.print("Main mainObj;").newline
    body.print("init_Main(&mainObj,&simulator);").newline
    body.print("/* main loop */").newline
    compileDumpObjHeader(body, "mainObj.", mainObj, prog)
    compileDumpObjHeader(body, "simulator.", magicObj, prog)
    body.print("printf(\"\\n\");").newline
    body.print("while (simulator.time_0 < simulator.endTime_0) {").newline.indent(2)
    body.print("simulator.stepType_0 = DISCRETE;").newline
    body.print("do {").newline.indent(2)
    compileDumpObj(body, "mainObj.", mainObj, prog)
    compileDumpObj(body, "simulator.", magicObj, prog)
    body.print("printf(\"\\n\");").newline
    body.print("somethingChanged = 0;").newline
    body.print("step_Main(&mainObj);").newline
    body.indent(-2).print("} while (somethingChanged);").newline
    body.print("simulator.stepType_0 = CONTINUOUS;").newline
    body.print("step_Main(&mainObj);").newline 
    compileDumpObj(body, "mainObj.", mainObj, prog)
    compileDumpObj(body, "simulator.",magicObj, prog)
    body.print("printf(\"\\n\");").newline
    body.print("simulator.time_0 += simulator.timeStep_0;").newline
    body.indent(-2).print("}").newline
    body.print("/* main loop end */").newline
    body.print("return 0;").newline
    Collector.newFun("main", Nil, "int", body.toString, "")

    Collector.writeOut

    gnuPlot(magicObj, mainObj)
  }

  def gnuPlot(magicObj: Object, mainObj: Object) {
    val out = new java.io.PrintWriter(new java.io.FileWriter("model.gpl"))
    out.println("set terminal png")
    out.println("set ylabel \"Time\"")
    val magicFields = getFilteredSortedFields(magicObj)
    val mainFields = getFilteredSortedFields(mainObj)
    val timeIdx = mainFields.size + magicFields.indexWhere { case (n,_,_,_) => n.x == "time" } + 1
    var idx = 1
    mainFields.foreach{ case (name, arrayIdx, f, _) =>
      if (f == "%f") {
        out.println()
        out.println("set xlabel \"" + name.x + "'" * name.primes + arrayPart(arrayIdx) +"\"")
        val arrayPartForFn = arrayIdx match { case Some(i) => "_" + i; case _ => ""}
        out.println("set output \"model_" + to_c_name(name) + arrayPartForFn + ".png\"")
        out.println("plot \"model.res\" using " + timeIdx + ":" + idx + " with lines notitle")
      }
      idx += 1            
    }
    out.close
  }


  def compileObj(obj: Object, p: Prog, magic: Object) {
    val cn =  getClassOf(obj).x;
    val cd = if (cn != "Simulator") classDef(getClassOf(obj), p) else null
    val classFields = if (cd != null) cd.fields else Nil
    val fieldTypes = MMap.empty[Name,CType]
    classFields.foreach { name => fieldTypes.update(name, CType("void *")) }

    // Hack, force any int fields to be doubles, they are likely meant
    // to be double even if the initial value is an int
    obj.fields.foreach { case (name, value) => 
      value match {
        case VLit(GInt(v)) => obj.fields.update(name, VLit(GDouble(v)))
        case _ => 
      }
    }                                 

    // First create a c struct for the object, the type for the struct
    // fields are based on the initial value in the object.  At the
    // same time set the types for any fields (it paramaters to the
    // object constructor) as they will be needed later.  
    Collector.newStruct(cn, obj.fields.map { case (name, value) => 
      val cname = to_c_name(name)
      val ctype = to_c_type(value)
      if (fieldTypes.contains(name)) {
        println("Updating " + name + " to " + ctype)
        fieldTypes.update(name, ctype)
      }
      CVar(cname, ctype) 
    } toList)

    // Now output the initialization function
    val args = List(CVar("obj", CType(cn + " *"))) ++
      classFields.map { name => CVar(to_c_name(name), fieldTypes(name)) }
    var body = new CompileWriter(2)
    obj.fields.foreach { 
      case (name, value) =>
        body.print("obj->" + to_c_name(name) + " = ")
        if (fieldTypes.contains(name))
            body.print(to_c_name(name))
        else
          body.print(to_c_value(value))
        body.print(";").newline                  
    }
    Collector.newFun("init_" + cn, args, "void", body.toString)

    // Now output a function for the object actions
    if (cn != "Simulator") {
      val env = HashMap((self, VObjId(Some(obj))))
      body = new CompileWriter(2)
      compileActions(body, cd.body, env, p, magic)
      Collector.newFun("step_" + cn, List(CVar("obj", CType(cn + " *"))), "void", body.toString)
    }
  }

  def compileActions(cr: CompileWriter, as: List[Action], env: Env, p: Prog, magic: Object) =
    as.foreach {a => compileAction(cr, a, env, p, magic)}

  def compileAction(cr: CompileWriter, a: Action, env: Env, p: Prog, magic: Object) {
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
        val VLit(gv) = evalExpr(s, p, env)
        cr.print(CVar("switchVal", to_c_type(gv)).toDecl + " = ")
        cr.print(compileExpr(s, p, env))
        cr.print(";").newline
        var first = true
        cls.foreach { cl => 
          if (!first)
            cr.indent(-2).print("} else ")
          cr.print("if (" + c_cmp(gv, "switchVal", to_c_value(cl.lhs))  + ") {").newline.indent(2)
          compileActions(cr, cl.rhs, env, p, magic)
          first = false
        }
        cr.indent(-2).print("} else { abort(); }").newline
        cr.indent(-2).print("}").newline
      case Discretely(da) =>
        cr.print("if (simulator.stepType_0 == DISCRETE) {").newline.indent(2)
        compileDiscreteAction(cr, da, env, p, magic)
        cr.indent(-2).print("}").newline
      case Continuously(ca) =>
        cr.print("if (simulator.stepType_0 == CONTINUOUS) {").newline.indent(2)
        compileContinuousAction(cr, ca, env, p, magic)
        cr.indent(-2).print("}").newline
    }
  }

  def compileDiscreteAction(cr: CompileWriter, a: DiscreteAction, env: Env, p: Prog, magic: Object) =
    a match {
      case Assign(ed@Dot(e, x), t) =>
        val VObjId(Some(o)) = evalExpr(e, p, env)
        val lhs = compileExpr(ed, p, env) // FIXME: Verify this is doing the right thing
        val rhs = compileExpr(t, p, env)
        o.fields(x) match {
          case VLit(gv) => 
            cr.print("if (" + c_cmp_inv(gv, lhs,rhs) + " ) {").newline.indent(2)
            cr.print(lhs + " = " + rhs + ";").newline
            cr.print("somethingChanged = 1;").newline
            cr.indent(-2).print("}").newline
          case VVector(l) => 
            cr.print(mkCallVectorAssignIfChanged(l.size, lhs, rhs, p, env) + ";").newline
        }
      case _ => 
        cr.print("/*Unimplemented*/").newline
    }

  def compileContinuousAction(cr: CompileWriter, a: ContinuousAction, env: Env, p: Prog, magic: Object) : Unit =
    a match {
      case EquationT(Dot(e, x), t) =>
        // val VObjId(Some(a)) = evalExpr(e, p, env)
        // FIXME: Assuming single object case
        cr.print("obj->" + to_c_name(x) + " = ");
        cr.print(compileExpr(t, p, env))
        cr.print(";").newline
      case EquationI(Dot(e, x), t) =>
        //val dt = getTimeStep(magic)
        val VObjId(Some(id)) = evalExpr(e, p, env)
        //val vt = evalExpr(t, p, env)
        val lhs = getField(id, x)
        // FIXME: Assuming single object case
        lhs match {
          case VLit(d) =>
            cr.print("obj->" + to_c_name(x) + " += ")
            cr.print(compileExpr(t, p, env))
            cr.print(" * " + "simulator.timeStep_0")
            //VLit(GDouble(extractDouble(d) + extractDouble(vt) * dt))
          case VVector(u) =>
            cr.print(mkCallVectorContinuous(u.size, "obj->" + to_c_name(x), compileExpr(t, p, env), p, env) + ";").newline
          case _ =>
            throw BadLhs()
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
    if (!Collector.haveSym(typeName))
      Collector.newStruct(typeName, List(CVar("d", CType("double", arrayPart(sz)))))
    typeName
  }

  def compileExpr(e: Expr, p: Prog, env: Env): String = {
    e match {
      case Lit(i)        => to_c_value(i)
      case ExprVector(l) => 
        "(" + vectorType(l.size) + "){" + l.map{e => compileExpr(e,p,env)}.mkString(", ") + "}"
      case Var(n)        => "obj->" + to_c_name(n)
      case Dot(v, Name("children", 0)) => unimplemented(e)
        //val VObjId(Some(id)) = eval(env, v)
        ///id synchronized { VList((id.children map VObjId[ObjId]).toList) }
        //VList((id.children map (c => VObjId(Some(c)))).toList)
      /* e.f */
      case Dot(Var(Name("self", 0)), f) => "obj->" + to_c_name(f)
      case Dot(e, f) => 
        compileExpr(e, p, env) + "->" + to_c_name(f)
        //val VObjId(Some(id)) = eval(env, e)
        //cr.print("obj->" + e + "->" + f) // FIXME NOW (Once I find a test case)
      /* x && y */
      case Op(Name("&&", 0), x :: y :: Nil) => compileToCBinOp("&&", x, y, p, env)
      case Op(Name("||", 0), x :: y :: Nil) => compileToCBinOp("||", x, y, p, env)
      case Op(Name(op, 0), args) => compileOp(e, op, args map (evalExpr(_, p, env)), args,p,env)
      /* sum e for i in c st t */
      case Sum(e, i, c, t) => unimplemented(e)
      case CpuSpin(n) => unimplemented(e)
      case TypeOf(cn) => unimplemented(e)
    }
    //cr.print("/*="+ e + "*/ ") 
  }

  def compileOp(exp: Expr, op:String, argTypes:List[Val], args: List[Expr], p: Prog, env: Env) : String = {
    (op,argTypes,args) match {
       case ("==", _, x :: y :: Nil) => compileToCBinOp("==", x, y, p, env)
       case ("~=", _, x :: y :: Nil) => compileToCBinOp("!=", x, y, p, env)
       case ("_:_:_", VLit(GInt(s))::VLit(GInt(d))::VLit(GInt(e))::Nil, _) =>
         unimplemented(exp)
       case (_, VLit(xv)::Nil, x :: Nil) =>
         compileUnaryOp(op,xv,x,p,env)
       case (_,VLit(xv)::VLit(yv)::Nil, x :: y :: Nil) =>  
         compileBinOp(op,xv,yv,x,y,p,env)
       case (_, VVector(u)::Nil, x :: Nil) =>
         compileUnaryVectorOp(op,u,x,p,env)
       case (_, VVector(u)::VVector(v)::Nil, x :: y :: Nil) =>
         compileBinVectorOp(op,u,v,x,y,p,env)
       case (_, VLit(xv)::VVector(u)::Nil, x :: y :: Nil) =>
         compileBinScalarVectorOp(op,xv,u,x,y,p,env)
       case (_, VVector(u)::VLit(xv)::Nil, x :: y :: Nil) =>
         compileBinVectorScalarOp(op,u,xv,x,y,p,env)
       case (_, VList(u)::Nil, _) =>
         unimplemented(exp)
       case _ =>
         throw UnknownOperator(op)    
    }
  }

  /* purely functional unary operator evaluation 
   * at the ground values level */
  def compileUnaryOp(f:String, vx:GroundValue, xe: Expr, p: Prog, env: Env) : String = {
    def implem(f:String, x:Double) = f match {
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
    (f, vx) match {
      case ("not", GBool(x))   => "(! " + compileExpr(xe, p, env) + ")"
      case ("abs", GInt(i))    => compileToCFunCall("abs", List(xe), p, env)
      case ("-",   GInt(i))    => "(- " + compileExpr(xe, p, env) + ")"
      case ("abs", GDouble(x)) => compileToCFunCall("fabs", List(xe), p, env)
      case ("-",   GDouble(x)) => "(- " + compileExpr(xe, p, env) + ")"
      case ("round", GDouble(x)) => compileToCFunCall("round", List(xe), p, env)
      case _                   => implem(f, extractDouble(vx))
    }
  }

  /* purely functional binary operator evaluation 
   * at the ground values level */
  def compileBinOp(f:String, vx:GroundValue, vy:GroundValue, xe: Expr, ye: Expr, p: Prog, env: Env) = {
    def implem1(f:String, x:Int, y:Int) = f match {
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
    def implem2(f:String, x:Double, y:Double) = f match {
      case "+" => compileToCBinOp("+", xe, ye, p, env)
      case "-" => compileToCBinOp("-", xe, ye, p, env)
      case "*" => compileToCBinOp("*", xe, ye, p, env)
      case "^" => compileToCFunCall("pow", List(xe, ye), p, env)
      case "/" => compileToCBinOp("/", xe, ye, p, env)
      case "atan2" => compileToCFunCall("atan2", List(ye, xe), p, env) 
      /* ^^ yes the order is reversed, not a bug */
      case _ => throw UnknownOperator(f)
    }
    def implem3(f:String, x:Int, y:Int) = f match {
      case "<"  => compileToCBinOpForceInt("<", xe, ye, p, env)
      case ">"  => compileToCBinOpForceInt(">", xe, ye, p, env)
      case "<=" => compileToCBinOpForceInt("<=", xe, ye, p, env)
      case ">=" => compileToCBinOpForceInt(">=", xe, ye, p, env)
    }
    def implem4(f:String, x:Double, y:Double) = f match {
      case "<" => compileToCBinOp("<", xe, ye, p, env)
      case ">" => compileToCBinOp(">", xe, ye, p, env)
      case "<=" => compileToCBinOp("<=", xe, ye, p, env)
      case ">=" => compileToCBinOp(">=", xe, ye, p, env)
    }
    (f, vx, vy) match {
      case (">="|"<="|"<"|">", GInt(n), GInt(m)) => implem3(f,n,m)
      case ("<"|">"|"<="|">=", _, _) => implem4(f,extractDouble(vx),extractDouble(vy))
      case ("+"|"-"|"*"|"<<"|">>"|"&"|"|"|"%"|"xor", GInt(n), GInt(m)) => implem1(f,n,m)
      case _  => implem2(f, extractDouble(vx), extractDouble(vy))
    }
  }

  def compileUnaryVectorOp(op: String,u: List[Val],xe: Expr, p: Prog, env: Env) : String = {
    op match {
      //case "length" => VLit(GInt(u.length)) 
      case "norm" => "/* unimplemented: norm */"
      case _ => throw InvalidVectorOp(op)
    }
  }
  
  def compileBinVectorOp(op: String,u: List[Val],v: List[Val],
                         xe: Expr, ye: Expr, p: Prog, env: Env) : String = {
    val sz = u.size
    def mkCall(fname: String, f: (String, String) => String) = {
      val fullName = "bin_vector_" + fname + "_" + sz
      mkCallBinVectorFun(fullName, sz, vectorType(sz), vectorType(sz), xe, ye, f("x[i]", "y"), p, env)
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

  def compileBinScalarVectorOp(op: String, xv: GroundValue,u: List[Val], 
                               xe: Expr, ye: Expr, p: Prog, env: Env) : String = {
    op match {
      case "+" => compileBinVectorScalarOp(op,u,xv,ye,xe,p,env)
      case "*" => compileBinVectorScalarOp(op,u,xv,ye,xe,p,env)
      case _ => throw InvalidScalarVectorOp(op)
    }
  }
  
  def compileBinVectorScalarOp(op: String,u: List[Val],xv: GroundValue, 
                               xe: Expr, ye: Expr, p: Prog, env: Env) : String = {
    val sz = u.size
    def mkCall(fname: String, f: (String, String) => String) = {
      val fullName = "bin_vector_scalar_" + fname + "_" + sz
      mkCallBinVectorFun(fullName, sz, vectorType(sz), "double", xe, ye, f("x[i]", "y"), p, env)
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
                         xType: String, yType: String,
                         xe: Expr, ye: Expr, 
                         loopBodyExpr: String, p: Prog, env: Env) : String = {
    if (!Collector.haveSym(fullName)) {
      val body = new CompileWriter(2)
      body.print(vectorType(sz) +" res;").newline
      body.print("int i;").newline
      body.print("for (i = 0; i != " + sz + "; ++i) {").newline.indent(2)
      body.print("res.d[i] = " + loopBodyExpr + ";").newline
      body.indent(-2).print("}").newline
      body.print("return res")
      Collector.newFun(fullName, List(CVar("x", CType(xType)), CVar("y", CType(yType))),
                       vectorType(sz), body.toString, "static inline ")
    }
    return fullName + "(" + compileExpr(xe,p,env) + "," + compileExpr(ye,p,env) + ")";
  }

  def mkCallVectorAssignIfChanged(sz: Int, xe: String, ye: String, p: Prog, env: Env) : String = {
    val fullName = "vector_assign_if_changed_" + sz;
    if (!Collector.haveSym(fullName)) {
      val body = new CompileWriter(2)
      body.print("int i = 0;").newline
      body.print("while (i != " + sz + ") {").newline.indent(2)
      body.print("if (x->d[i] != y.d[i]) {").newline.indent(2)
      body.print("*x = y;").newline
      body.print("somethingChanged = 1;").newline
      body.print("return;").newline
      body.indent(-2).print("} else {").indent(2).newline
      body.print("++i;").newline
      body.indent(-2).print("}").newline
      body.indent(-2).print("}").newline
      Collector.newFun(fullName, List(CVar("x", CType((vectorType(sz) + " *"))), 
                                      CVar("y", CType(vectorType(sz)))),
                       "void", body.toString, "static inline ")
    }
    return fullName + "(" + "&" + "(" + xe + ")" + "," + ye + ")";
  }

  def mkCallVectorContinuous(sz: Int, xe: String, ye: String, p: Prog, env: Env) : String = {
    val fullName = "vector_continuous_" + sz;
    if (!Collector.haveSym(fullName)) {
      val body = new CompileWriter(2)
      body.print("int i;").newline
      body.print("for (i = 0; i != " + sz + "; ++i) {").newline.indent(2)
      body.print("x->d[i] += y.d[i] * simulator.timeStep_0;").newline
      body.indent(-2).print("}").newline
      Collector.newFun(fullName, List(CVar("x", CType((vectorType(sz) + " *"))), 
                                      CVar("y", CType(vectorType(sz)))),
                       "void", body.toString, "static inline ")
    }
    return fullName + "(" + "&" + "(" + xe + ")" + "," + ye + ")";
  }

  def mkCallVectorDot(sz: Int, xe: Expr, ye: Expr, p: Prog, env: Env) : String = {
    val fullName = "vector_dot_" + sz;
    if (!Collector.haveSym(fullName)) {
      val body = new CompileWriter(2)
      body.print("double res = 0;").newline
      body.print("int i;").newline
      body.print("for (i = 0; i != " + sz + "; ++i) {").newline.indent(2)
      body.print("res += x.d[i] * y.d[i];").newline
      body.indent(-2).print("}").newline
      body.print("return res;").newline
      Collector.newFun(fullName, List(CVar("x", CType(vectorType(sz))), CVar("y", CType(vectorType(sz)))),
                       "double", body.toString)
    }
    return fullName + "(" + compileExpr(xe,p,env) + "," + compileExpr(ye,p,env) + ")";
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
      case Some(i) => cname + ".d[" + i + "]"
      case _ => cname
    }
  }

  def to_c_string(str: String) : String = "\"" + str + "\""

  def to_c_type(value: GroundValue): CType = value match {
    case GInt(_)    => CType("double")
    // ^^ a double is far more likely than an int, so just assume
    // a double for now rather than getting it wrong.  Making it
    // an int will be considered an optimizaton :) - kevina
    case GDouble(_) => CType("double")
    case GBool(_)   => CType("Boolean")
    case GStr(_)    => CType("const char *")
    case v          => CType("/*"+v+"*/double") // FIXME
  }

  def to_c_type(value: Val): CType = value match {
    case VLit(v) => to_c_type(v)
    case VObjId((Some(o))) => CType(getClassOf(o).x + " *")
    case VClassName(ClassName(n)) => CType("const char *")
    case VStepType(_) => CType("StepType")
    case VVector(l) => CType(vectorType(l.size))
    case n => CType("/*"+ n + "*/ void *") // FIXME
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
      case VStepType(Discrete()) => "DISCRETE"
      case VStepType(Continuous()) => "CONTINUOUS"
      case VVector(l) => "(" + vectorType(l.size) + ")" + "{" + l.map{v => to_c_value(v)}.mkString(", ") + "}"
      case _ => "NULL" // FIXME
    }
  }

  def c_cmp (theType: GroundValue, x: String, y: String) = theType match {
    case GStr(_) => "strcmp(" + x + ", " + y + ")" + "== 0"
    case _ => x + " == " + y;
  }

  def c_cmp_inv (theType: GroundValue, x: String, y: String) = theType match {
    case GStr(_) => "strcmp(" + x + ", " + y + ")" + "!= 0"
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
    "(" + compileExpr(x, p, env) + cop + compileExpr(y, p, env) + ")"
  }

  def compileToCBinOpForceInt(cop: String, x: Expr, y: Expr, p: Prog, env: Env) : String = {
    "(" + "(int)(" + compileExpr(x, p, env) + ")" + cop + "(int)(" + compileExpr(y, p, env) + "))"
  }

  def compileToCFunCall(funName: String, args: List[Expr], p: Prog, env: Env) : String = {
    funName + "(" + args.map{arg => compileExpr(arg, p, env)}.mkString(", ") + ")"
  }

  def compileDumpObjHeader(cr: CompileWriter, prefix: String, o: Object, p: Prog) : Unit = {
    val cn =  getClassOf(o).x;
    val fields = getFilteredSortedFields(o);
    cr.print("printf(\"%s\", \"");
    cr.print(fields.map{case (n,i,_,_) => cn + "." + n.x + "'"*n.primes+arrayPart(i)}.mkString(" "))
    cr.print(" \");").newline
  }

  def compileDumpObj(cr: CompileWriter, prefix:String, o: Object, p: Prog) : Unit = {
    val fields = getFilteredSortedFields(o);
    cr.print("printf(\"");
    cr.print(fields.map( {case (_,_,f,_) => f}).mkString(" "))
    cr.print(" \", ")
    cr.print(fields.map{case (n,i,_,f) => f(prefix + to_c_name(n,i))}.mkString(","))
    cr.print(");").newline
  }

  def getFilteredSortedFields(o: Object) : List[(Name,Option[Int],String,String => String)] = {
    val res = o.fields.toList.sortWith{(x,y) => x._1 < y._1}.map { case (name, value) =>
      value match {
        case VLit(GDouble(_)) => List((name, None, "%f", {n:String => n}))
        case VLit(GStr(_)) => List((name, None, "%s", {n:String => n}))
        case VStepType(_) => List((name, None, "%s", 
                              {n:String => n + "== DISCRETE ? \"@Discrete\" : \"@Continuous\""}))
        case VVector(l) => l.indices.map{i => (name, Some(i), "%f", {n:String => n})}.toList
        case _ => Nil
      }
    }
    List.flatten(res)
  }

}
