package acumen
import Pretty._
import interpreters.enclosure2015.Common._
import scala.util.parsing.input.{Position,Positional,NoPosition,OffsetPosition}

object Errors {

  abstract class AcumenError extends RuntimeException

  abstract class PositionalAcumenError extends AcumenError with Positional {
    override def getMessage = 
      if (pos == NoPosition)
        mesg
      else
        pos.toString + ": " + mesg + "\n" + pos.longString
    def mesg : String
  }

  case class ParseError(message:String) extends PositionalAcumenError {
    override def mesg = message
  }
  case class ClassDefinedTwice(cn:ClassName) extends PositionalAcumenError {
    override def mesg = 
      "Model " + pprint(cn) + " is defined twice."
  }
  case class ObjectDeleted(cid:CId) extends PositionalAcumenError {
    override def mesg = 
      "Object " + cid + " is deleted."
  }
  case class ClassIncludedTwice(cn:ClassName, pos1: List[Position], pos2: List[Position]) extends PositionalAcumenError {
    override def mesg = 
      "Model " + pprint(cn) + " included twice."
    override def getMessage = 
        super.getMessage + "\n" + locationTrace("First location", pos1) + locationTrace("Second location", pos2)
    def locationTrace(descr: String, pos: List[Position]) : String = {
      pos.head.toString + ": " + descr + "\n" +
      pos.tail.map{p => p.toString + ": included from\n"}.mkString("")
    }
  }
  case class ClassOverride(cn:String, libDir: String, curDir: String) extends PositionalAcumenError {
    override def mesg: String =
      "Model " + cn + " is defined in " + libDir + " , and overwrite in " + curDir
  }
  case class NoInstanceFound(cn:ClassName) extends PositionalAcumenError {
    override def mesg = 
      "Found no instance of model " + pprint(cn) + "."
  }
  case class ClassNotDefined(cn:ClassName) extends PositionalAcumenError {
    override def mesg = 
      "Model "+pprint(cn)+" is not defined."
  }
  case class NotAnObject(v:Value[_]) extends PositionalAcumenError {
    override def mesg =
      pprint(v) + " is not an object."
  }
  case class NotAClassName(v:Value[_]) extends PositionalAcumenError {
    override def mesg =
      pprint(v) + " is not a model name."
  }
  case class GroundConversionError(gv:GroundValue, into:String) extends PositionalAcumenError {
    override def mesg = 
      "Cannot convert " + pprint(gv) + " into a " + into + "."
  }
  case class ConversionError(v:Value[_], into:String) extends PositionalAcumenError {
    override def mesg = 
      "Cannot convert " + pprint(v) + " into a " + into + "."
  }
  case class NotACollection(v:Value[_]) extends PositionalAcumenError {
    override def mesg = 
      pprint(v) + " is not a vector or a list."
  }
  case class UnknownOperator(op:String) extends PositionalAcumenError {
    override def mesg = 
      "Unknown operator " + op + "."
  }
  case class InvalidPrintName(nm: Value[_]) extends PositionalAcumenError {
    override def mesg =
      pprint(nm) + " is not a valid value name to print. The value name should be a string."
  }
  case class CrossProductError() extends PositionalAcumenError {
    override def mesg = 
      "Cross product only defined over vectors of size 3."
  }
  case class InvalidVectorVectorOp(op:String) extends PositionalAcumenError {
    override def mesg = 
      op + " is not a valid vector-vector operation."
  }
  case class LengthVectorVectorOp(u:List[Value[_]],v:List[Value[_]]) extends PositionalAcumenError {
    override def mesg = 
      "Vector " + u.mkString(",")  + " and vector " + v.mkString(",") + " has different length."
  }
  case class LengthVectorVectorOpExpr(u:ExprVector,v:ExprVector) extends PositionalAcumenError {
    override def mesg = 
      "Vector " + pprint(u.asInstanceOf[Expr])  + " and vector " + pprint(v.asInstanceOf[Expr]) + " has different length."
  }
  case class InvalidVectorOp(op:String) extends PositionalAcumenError {
    override def mesg = 
      op + " is not a valid operation over vectors."
  }
  case class InvalidListOp(op:String) extends PositionalAcumenError {
    override def mesg = 
      op + " is not a valid operation over lists."
  }
  case class InvalidScalarVectorOp(op:String) extends PositionalAcumenError {
    override def mesg = 
      op + " is not a valid scalar-vector operation."
  }
  case class InvalidVectorScalarOp(op:String) extends PositionalAcumenError {
    override def mesg = 
      op + " is not a valid vector-scalar operation."
  }
  case class InvalidDynSet(r:String) extends PositionalAcumenError {
    override def mesg = 
      "\"" + r + "\" is not a valid dynamical set type. " + 
        s"Use one of: ${allDynSets.productIterator.map("\"" + _ + "\"").mkString(", ")}."
  }
  case class InvalidReorganization(r:String) extends PositionalAcumenError {
    override def mesg = 
      "\"" + r + "\" is not a valid reorganization. " + 
        s"Use one of: ${allReorganizations.productIterator.map("\"" + _ + "\"").mkString(", ")}."
  }
  case class InvalidBranchMergingStrategy(r:String) extends PositionalAcumenError {
    override def mesg = 
      "\"" + r + "\" is not a valid branch merging strategy. " + 
        s"Use one of: ${allBranchMergingStrategies.productIterator.map("\"" + _ + "\"").mkString(", ")}."
  }
  case class InvalidTimeStepConfiguration(r: List[String]) extends PositionalAcumenError {
    override def mesg =
      "Invalid time step configuration: " + r.mkString(", ") + ". " +
      "Either set only timeStep (to use a fixed step) or both minTimeStep and maxTimeStep but not timeStep (to use adaptive step)."
  }
  case class ConstructorArity(cd:ClassDef,got:Int) extends PositionalAcumenError {
    override def mesg = {
      val cn = cd.name
      val expected = cd.fields.length
      val prefix = 
        if (got > expected) "Too many "
        else if (expected > got) "Not enough "
        else throw ShouldNeverHappen()
      prefix + "arguments in the construction of " + 
        pprint(cn) + "."
    }
  }
  case class IndexOutOfBounds(i: List[Int]) extends PositionalAcumenError {
    override def mesg = "Index " + (if (i.length == 1) i.head else i.mkString("(",",",")")) + " is out of bounds."
  }
  case class CantIndex() extends PositionalAcumenError {
    override def mesg = "Can only index into vectors."
  }
   case class IndexNoMatch(v: List[Value[_]]) extends PositionalAcumenError {
    override def mesg = "Can't do index (" + v.foldLeft("")((r,x) => pprint(x) + ",") + ")."
  }
  
  case class ExpectedInteger(v: Value[_]) extends PositionalAcumenError {
    override def mesg = "Expected integer but got " + pprint(v) + "."
  }
  case class VariableNotDeclared(x:Name) extends PositionalAcumenError {
    override def mesg =
      "Variable " + pprint(x) + " is not declared."
  }
  case class AccessDenied[A](id:A,self:A,children:List[A]) extends PositionalAcumenError {
    override def mesg = 
      "Object #"+id+" is not self (i.e. #"+self+") nor a child of self" + 
         (if (children.length > 0) " (i.e. "+ children.map("#"+_).mkString(", ")+")"
          else "") + "."
  }
  case class NotAChildOf[A](id:A, obj:A) extends PositionalAcumenError {
    override def mesg = 
      "Object #"+id+" is not a child of #"+obj+"."
  }
  case class NoMatch(gv:GroundValue) extends PositionalAcumenError {
    override def mesg = 
      "No case matching " + pprint(gv) + "."
  }
  case class FunctionOutOfRange(f:String, x:Double) extends PositionalAcumenError {
    override def mesg =
    "Argument " + x + "for Function "+ f + " is out of range."
  }
  sealed abstract class DuplicateAssingment extends PositionalAcumenError {
    val x: Name
    var otherPos : Position = NoPosition
    def setOtherPos(o: Position) : this.type = {
      if (pos == NoPosition) pos = o
      else otherPos = o
      return this
    }
    def mesg(kind: String) = 
      "Repeated" + kind + "assignment to variable (" + x.x + "'" * x.primes + ") is not allowed."
    override def getMessage = super.getMessage + (
      if (otherPos == NoPosition) "" 
      else "\n" + otherPos.toString + ": other location\n" + otherPos.longString)
  }
  case class DuplicateAssingmentUnspecified(x:Name) extends DuplicateAssingment {
    override def mesg = super.mesg(" ")
  }
  case class DuplicateDiscreteAssingment(x:Name) extends DuplicateAssingment {
    override def mesg = super.mesg(" discrete ")
  }
  case class DuplicateContinuousAssingment(x:Name) extends DuplicateAssingment {
    override def mesg = super.mesg(" continuous ")
  }
  case class ContinuousAssignmentToSimulator(rhs: Expr) extends PositionalAcumenError {
    override def mesg() = 
      "Only discrete assingments to simulator parameters are allowed."
    pos = rhs.pos
  }
  case class BadLhs() extends PositionalAcumenError {
    override def mesg = 
      "The left-hand side of an assignment must be of the form 'e.x'."
  }
  case class BadRhs(message: String) extends PositionalAcumenError {
    override def mesg = "Invalid assignment: " + message
  }
  case class BadPreLhs() extends PositionalAcumenError {
    override def mesg = 
      "The left-hand side of an equation must be a field."
  }
  case class BadMove() extends PositionalAcumenError {
    override def mesg = 
      "Move statements must have the form 'move o1.x o2'."
  }
  case class UnsupportedTypeError(kind: String, id: String, value: CValue) extends PositionalAcumenError {
    override def mesg =
      s"Unsupported $kind: $id = ${pprint(value)}"
  }
  case class UnsupportedTypeChangeError(f: Name, id: CId, clazz: ClassName, vOld: GValue, vNew: GValue, reason: String) extends PositionalAcumenError {
    override def mesg =
      s"Can not change value of (${id.toString}:${Pretty pprint clazz}).${Pretty pprint f} from ${Pretty pprint vOld} of type ${vOld.getClass.getSimpleName} to ${Pretty pprint vNew} of type ${vNew.getClass.getSimpleName}. $reason."
    pos = vNew.pos
  }
  case class ContinuousDynamicsUndefined(o: CId, n: Name, idx: Option[List[Int]], className: String, time: Double) extends AcumenError {
    override def getMessage =
      "No equation was specified for (#" + o.cid.toString + " : " + className + ")." + n.x + (idx match {
        case None    => ""
        case Some(l) => "(" + l.map(x => x.toString).mkString(",") + ")"
      }) +
        " at time " + time + "."
  }
  case class AlgebraicLoop(first : ObjField, posIsSetPoint : Boolean = false,
                           haveLoop : Boolean = false, chain : List[(ObjField,Position)] = Nil) 
       extends PositionalAcumenError 
  {
    override def mesg = "Algebraic loop detected " + (if (posIsSetPoint) "while setting " else "while retrieving ") + first
    override def getMessage = 
      super.getMessage + "\n" + chain.map{case (f,p) =>
        val msg = "while retrieving " + f
        p match {
           case NoPosition => msg 
           case _ => p.toString + ": " + msg + "\n" + p.longString
        }                                  
      }.mkString("\n")

    def addToChain(f: ObjField, p : Position)  = 
       if (haveLoop)           this
       else if (chain.isEmpty) copy(chain = (f,p)::Nil).setPos(pos).setPos(p)
       else if (f == first)    copy(haveLoop = true).setPos(pos)
       else                    copy(chain = (f,p)::chain).setPos(pos)
  }

  case class HypothesisFalsified(s: String, counterExample: Option[(Double, Map[Dot, CValue])] = None) extends PositionalAcumenError {
    override def mesg = 
      "Hypothesis \"" + s + "\" falsified." + (counterExample match {
        case None => ""
        case Some((time,m)) => 
          s"\nAt time $time: " + m.map{case (d,v) => 
            Pretty.pprint(d.obj) + "." + Pretty.pprint(d.field) + " = " + Pretty.pprint(v)}.mkString(", ")}) + "."
  }

  /* UI errors */

  case class BadTimeType() extends AcumenError {
    override def getMessage = 
      "Simulation's time is not a double."
  }
  case class BadStepTypeType() extends AcumenError {
    override def getMessage = 
      "Simulation's stepType is not a step type."
  }
  case class InvalidSimulatorParameterValue(msg: String) extends AcumenError {
    override def getMessage = 
      "Invalid simulator parameter value: " + msg
  }
  case class BadUiTransition(message:String) extends AcumenError {
    override def getMessage = message
  }

  /* Command-line errors */

  case class DisabledSemantics(theString: String) extends PositionalAcumenError {
    override def mesg = 
      "Semantics disabled in this release: " + theString
  }

  case class UnrecognizedSemanticsString(theString: String) extends PositionalAcumenError {
    override def mesg = 
      "Unrecognized semantics string: " + theString
  }

  case class UnrecognizedTransformation(theString: String) extends AcumenError {
    override def getMessage = 
      "Unrecognized pass/transformation: " + theString
  }

  /* special errors */

  case class ShouldNeverHappen() extends AcumenError {
    override def getMessage = 
     "The \"impossible\" has just happened! " +
     "Please report a bug at http://code.google.com/p/acumen-language/issues/"+
     "including the present program."
  }
 
  case class _3DNameError(v:Value[_]) extends AcumenError {
    override def getMessage = 
      pprint(v) + " is not a valid 3D object's name." 
  }
   case class _3DSizeError(v:Value[_]) extends AcumenError {
    override def getMessage = 
			pprint(v) + "is not a valid 3D object's size variable"
  }
  case class _3DSphereSizeError() extends AcumenError {
    override def getMessage = 
      "Sphere's size should be a number"
  }
  
  case class _3DParaError(n:String) extends AcumenError {
    override def getMessage = 
      n + " is not a valid _3D parameter"
  }
  case class _3DCylinderSizeError() extends AcumenError {
    override def getMessage = 
      "Cylinder's size should be a vector with 2 elements"
  }
  case class _3DConeSizeError() extends AcumenError {
    override def getMessage = 
      "Cone's size should be a vector with 2 elements"
  }
  case class _3DBoxSizeError() extends AcumenError {
    override def getMessage = 
      "Box's size should be a vector with 3 elements"
  }
  case class _3DTextSizeError() extends AcumenError {
    override def getMessage = 
      "3D text's size should be a number"
  }

  case class _3DTriangleSizeError() extends AcumenError {
    override def getMessage =
      "3D triangle's size should be a vector with 3 vectors"
  }
  
	 case class _3DVectorError(v:Value[_], n:String) extends AcumenError {
    override def getMessage = 
			pprint(v) + "is not a valid 3D object's " + n + " variable."
  }
 case class _3DError(v:Value[_]) extends AcumenError {
    override def getMessage = 
			pprint(v) + " is not a valid 3D object"
 }

  case class _3DViewDuplicateError(id: CId, lastID: CId) extends AcumenError {
    override def getMessage =
      "_3DView can not be defined in both Object #" + id + " and Object #" + lastID + "."
  }

  case class FromJSONError(s:String) extends AcumenError {
    override def getMessage =
      "fromJSON failed with input: " + s
  }

  case class NewPlotEnclosureError() extends AcumenError {
    override def getMessage = 
      "New Plot does not support enclosures"
  }

  case class _3DObjectPathError(s:String) extends AcumenError {
    override def getMessage =
      s + " folder is not in _3D directory."
  }

  case class _3DLoadFileError(s:String) extends AcumenError {
    override def getMessage =
      s + " in _3D directory."
  }
  
  case class NotImplemented(s:String) extends AcumenError {
    override def getMessage =
      s + " is not yet implemented."
  }
 
  case class UnsupportedPlotType(s:String) extends AcumenError {
    override def getMessage =
      s"The expression $s can not be plotted. Only variable names are supported in _plot."
  }

  case class NonexistentVariableForPlot(s:String, objName: String) extends AcumenError {
    override def getMessage =
      s"The variable name $s is not defined in model $objName."
  }

  case class DuplicatesForPlot(objName: String) extends AcumenError {
    override def getMessage =
      s"There are duplicates for plotting in model $objName."
  }
  
  /* Device Input Error */
  case class invalidInput(s: String) extends AcumenError {
    override def getMessage =
      s + " is an invalid input from device."
  }
  case class invalidDevice(id: Int) extends AcumenError {
    override def getMessage =
      id + " is an invalid device ID, please check the devices that connected to Acumen."
  }
  case class GEVarsNotEqualToEquations(equations:List[Equation], vars:List[Var]) extends AcumenError{
    override def getMessage = 
      "Found " + equations.length + " equations and " + vars.length + " indireced variables " + 
       vars.map(x => pprint(x.asInstanceOf[Expr])).mkString(",") + "\n" +
       equations.map(x => pprint(x.lhs) + " = " + pprint(x.rhs) + "\n").mkString("\n")
  }
  
  case class symbolicDifWithoutBTA (e:Expr) extends PositionalAcumenError{
    override def mesg = 
       "Support for symbolic differentiation needs to be enabled for this model to run. \n " + 
       "Enable the 'BTA' checkbox under 'Semantics' menu or rerun the model with --enable-bta"
    pos = e.pos
  }
  
  case class equationalWithoutBTA (a:Expr) extends PositionalAcumenError{
    override def mesg = 
       "Support for equaltional models needs to be enabled for this model to run. \n " + 
       "Enable the 'BTA' checkbox under 'Semantics' menu or rerun the model with --enable-bta"
    pos = a.pos
  }
  /* utility class */
  case class ObjField(o: CId, cn: String, f: Name) {
    override def toString = s"(#$o : $cn)." + pprint(f)
  }

}
