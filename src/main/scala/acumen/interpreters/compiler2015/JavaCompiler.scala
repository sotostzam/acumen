package acumen
package interpreters
package compiler2015

import Ordered.orderingToOrdered // Handle Scala 2.11 type system flakiness

case object JavaCompiler extends Compiler {
  
  def suffix = "java"
  
  def discreteStateName = "m"
  
  def continuousStateName = "s"

  def genArrayAccess(arrayName: String, index: Int, ir: IR) = 
    s"$arrayName[$index]"
  
  def genVariableName(ir: IR) = {
s"""
/* Variable names */
static String continuousVariableName(int i) {
  if (i < 0 || ${ir.continuousState.size} < i)
    throw new RuntimeException("No continuous variable name with index " + i);
  return 
    ${ir.continuousState.zipWithIndex.map { case (((cid, name), _), i) =>  
      parens("i == " + i) + " ? \"" + genName(name) + "\" :"
    }.mkString("\n    ")}
    "DUMMY";
}
"""
  }
  
  def genStringValues(ir: IR) =
    "/* String values */\n" +
    ir.stringValues.map{ case (s,v) => 
      s"static int string_${IR.toValidFunctionNamePart(s)}() { return $v; } // $s"
    }.mkString("\n")

  def genInitialStore(ir: IR) = {
s"""
/* Initial store */
static double[]
  ${stateToString(ir.continuousState, Continuous, ir)}
static int[]
  ${stateToString(ir.discreteState,   Discrete,   ir)}
static double [] sNext;
static int [] mNext;
static double t;
static boolean fixpoint;
"""
  }

  def genDiscreteStep(ir: IR) = {
s"""
/* Discrete step */
static void discreteStep() {
  /* Model */
${ir.modes.toList.zipWithIndex.map{ case ((guard,mode),i) =>
   s"  ${if (i == 0) "if" else "else if"} (" + genConjunction(guard, ir, Discrete) + ") {\n" +
    "    sNext = new double[]\n" +
    "      { " + ir.continuousState.zipWithIndex.map{ case (((cid,name),_),i) => 
                   mode.das.find{ da => da.selfCId == cid && da.lhs.field == name }
                           .map(da => genExpr(da.rhs)(ir, Discrete))
                           .getOrElse(s"s[$i]")
                 }.mkString("\n      , ") + "\n" +
    "      };\n" +
    "    mNext = new int[]\n" +
    "      { " + ir.discreteState.zipWithIndex.map{ case (((cid,name),_),i) => 
                   mode.das.find{ da => da.selfCId == cid && da.lhs.field == name }
                           .map(da => genExpr(da.rhs)(ir, Discrete))
                           .getOrElse(s"m[$i]")
                 }.mkString("\n      , ") + "\n" +
    "      };" +
    "  }"
  }.mkString("\n")}
}
"""
  }
  
  def genField(ir: IR) = {
s"""
/* Field */
static double[] f(double[] ${continuousStateName}, int[] ${discreteStateName}) {
  /* Model */
${ir.modes.toList.zipWithIndex.map{ case ((guard,mode), i) =>
   s"  ${if (i == 0) "if" else "else if"} (" + genConjunction(guard, ir, Continuous) + ") {\n" +
    "    return new double[]\n" +
    "      { " + ir.continuousState.zipWithIndex.map{ case (((cid,name),_),i) => 
                   mode.odes.find{ da => da.selfCId == cid && da.lhs.field == name }
                            .map(da => genExpr(da.rhs)(ir, Continuous))
                            .getOrElse("0.0")
                 }.mkString("\n      , ") + "\n" +
    "      };" +
    "  }"
  }.mkString("\n")}
  throw new RuntimeException("No continuous dynamics active at time " + t + "!");
}
"""
  }

  /* Utilities */
  
  def stateToString[A](state: Map[(CId,Name),A], k: StateKind, ir: IR) = {
    val (name, description) = k match {
      case Continuous => ("s","Continuous")
      case Discrete   => ("m","Discrete") 
    } 
    val vLens = state.values.map(_.toString.length)
    def vLine(v: String, o: CId, n: Name, stringValue: String) = 
      s"$v${" " * (vLens.max - v.length + 1)}// $o.${(Pretty pprint n) + stringValue}"
    if (vLens.isEmpty) 
      name + " = {};" 
    else
      s"""$name = ${" " * vLens.max} // $description
    { ${
        state.map {
          case ((o, n), v: Int) if k == Discrete =>
            vLine(v.toString, o, n, ir.stringIndices.get(v).map(" = \""+_+"\"") getOrElse "")
          case ((o, n), v) =>
            vLine(v.toString, o, n, "")
        }.mkString("\n    , ")
      }
    };"""    
  }
  
  /* Expressions */
  
  def genOp(f:Name, es:List[Expr])(implicit ir: IR, sk: StateKind) =
    (f,es) match {
      case (Name("not",0),x::Nil) => 
        "!" + parens(genExpr(x))
      case (Name("_:_:_",0), x::y::z::Nil) =>
        parens(genExpr(x) + ":" + genExpr(y) + ":" + genExpr(z))
      case (Name(op,0),x::y::Nil) if Parser.lexical.delimiters contains op => 
        (x,y) match {
          case (Lit(GBool(true)), _) => genExpr(y)
          case (_, Lit(GBool(true))) => genExpr(x)
          case _ => parens(genExpr(x) + " " + op + " " + genExpr(y))
        }
      case _ => 
        genName(f) + parens((es map genExpr).mkString(", "))
    }
  
  def genGroundValue(gv: GroundValue)(implicit ir: IR, sk: StateKind): String =
    gv match {
      case GInt(i)      => "%f".format(i.toDouble) //i.toString
      case GDouble(x)   => "%f".format(x)
      case GBool(b)     => b.toString
      case GStr(s)      => s"string_${IR.toValidFunctionNamePart(s)}(/* $s */)" //"\"" + s + "\""
      case GPattern(ls) => "(" + (ls map genGroundValue).mkString(", ") + ")"
      case _            => "??"
    }
  
}