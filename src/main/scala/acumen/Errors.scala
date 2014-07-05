package acumen
import Pretty._
import scala.util.parsing.input.{Position,Positional,NoPosition}

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

  case class ParseError(message:String) extends AcumenError {
    override def getMessage = message
  }
  case class ClassDefinedTwice(cn:ClassName) extends PositionalAcumenError {
    override def mesg = 
      "Class " + pprint(cn) + " is defined twice."
  }
  case class ClassIncludedTwice(cn:ClassName, pos1: List[Position], pos2: List[Position]) extends PositionalAcumenError {
    override def mesg = 
      "Class " + pprint(cn) + " included twice."
    override def getMessage = 
        super.getMessage + "\n" + locationTrace("First location", pos1) + locationTrace("Second location", pos2)
    def locationTrace(descr: String, pos: List[Position]) : String = {
      pos.head.toString + ": " + descr + "\n" +
      pos.tail.map{p => p.toString + ": included from\n"}.mkString("")
    }
  }
  case class NoInstanceFound(cn:ClassName) extends PositionalAcumenError {
    override def mesg = 
      "Found no instance of class " + pprint(cn) + "."
  }
  case class ClassNotDefined(cn:ClassName) extends PositionalAcumenError {
    override def mesg = 
      "Class "+pprint(cn)+" is not defined."
  }
  case class NotAnObject(v:Value[_]) extends PositionalAcumenError {
    override def mesg =
      pprint(v) + " is not an object."
  }
  case class NotAClassName(v:Value[_]) extends PositionalAcumenError {
    override def mesg =
      pprint(v) + " is not an class name."
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
  case class CrossProductError() extends PositionalAcumenError {
    override def mesg = 
      "Cross product only defined over vectors of size 3."
  }
  case class InvalidVectorVectorOp(op:String) extends PositionalAcumenError {
    override def mesg = 
      op + " is not a valid vector-vector operation."
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
  case class IndexOutOfBounds(i: Int) extends PositionalAcumenError {
    override def mesg = "Index " + i + " is out of bounds."
  }
  case class CantIndex() extends PositionalAcumenError {
    override def mesg = "Can only index into vectors."
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
  case class BadLhs() extends PositionalAcumenError {
    override def mesg = 
      "The left hand-side of an assignment must be of the form 'e.x'."
  }
  case class BadPreLhs() extends PositionalAcumenError {
    override def mesg = 
      "The left hand-side of an equation must be a field."
  }
  case class BadMove() extends PositionalAcumenError {
    override def mesg = 
      "Move statements must have the form 'move o1.x o2'."
  }
  case class ContinuousDynamicsUndefined(o: CId, n: Name, className: String, time: Double) extends AcumenError {
    override def getMessage = 
      "No equation was specified for (#" + o.cid.toString + " : " + className + ")." + n.x + " at time " + time + "."
  }
  case class HypothesisFalsified(s: String) extends PositionalAcumenError {
    override def mesg = 
      "Hypothesis \"" + s + "\" falsified."
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
  case class BadUiTransition(message:String) extends AcumenError {
    override def getMessage = message
  }

  /* Command-line errors */

  case class UnrecognizedInterpreterString(theString: String) extends AcumenError {
    override def getMessage = 
      "Unrecognized interpreter string: " + theString
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
  
	 case class _3DVectorError(v:Value[_], n:String) extends AcumenError {
    override def getMessage = 
			pprint(v) + "is not a valid 3D object's " + n + " variable."
  }
 case class _3DError(v:Value[_]) extends AcumenError {
    override def getMessage = 
			pprint(v) + " is not a valid 3D object"
  }

  case class FromJSONError(s:String) extends AcumenError {
    override def getMessage =
      "fromJSON failed with input: " + s
  }

  case class ObservesError(msg: String) extends AcumenError {
    override def getMessage = msg
  }
}
