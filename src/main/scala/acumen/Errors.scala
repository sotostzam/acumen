package acumen
import Pretty._

object Errors {

  sealed abstract class AcumenError extends Exception
  case class ParseError(message:String) extends AcumenError {
    override def getMessage = message
  }
  case class ClassDefinedTwice(cn:ClassName) extends AcumenError {
    override def getMessage = 
      "Class " + pprint(cn) + " is defined twice."
  }
  case class NoInstanceFound(cn:ClassName) extends AcumenError {
    override def getMessage = 
      "Found no instance of class " + pprint(cn) + "."
  }
  case class ClassNotDefined(cn:ClassName) extends AcumenError {
    override def getMessage = 
      "Class "+pprint(cn)+" is not defined."
  }
  case class NotAnObject(v:Value[_]) extends AcumenError {
    override def getMessage =
      pprint(v) + " is not an object."
  }
  case class GroundConversionError(gv:GroundValue, into:String) extends AcumenError {
    override def getMessage = 
      "Cannot convert " + pprint(gv) + " into a " + into + "."
  }
  case class ConversionError(v:Value[_], into:String) extends AcumenError {
    override def getMessage = 
      "Cannot convert " + pprint(v) + " into a " + into + "."
  }
  case class NotACollection(v:Value[_]) extends AcumenError {
    override def getMessage = 
      pprint(v) + " is not a vector or a list."
  }
  case class UnknownOperator(op:String) extends AcumenError {
    override def getMessage = 
      "Unknown operator " + op + "."
  }
  case class CrossProductError() extends AcumenError {
    override def getMessage = 
      "Cross product only defined over vectors of size 3."
  }
  case class InvalidVectorVectorOp(op:String) extends AcumenError {
    override def getMessage = 
      op + " is not a valid vector-vector operation."
  }
  case class InvalidVectorOp(op:String) extends AcumenError {
    override def getMessage = 
      op + " is not a valid operation over vectors."
  }
  case class InvalidListOp(op:String) extends AcumenError {
    override def getMessage = 
      op + " is not a valid operation over lists."
  }
  case class InvalidScalarVectorOp(op:String) extends AcumenError {
    override def getMessage = 
      op + " is not a valid scalar-vector operation."
  }
  case class InvalidVectorScalarOp(op:String) extends AcumenError {
    override def getMessage = 
      op + " is not a valid vector-scalar operation."
  }
  case class ConstructorArity(cd:ClassDef,got:Int) extends AcumenError {
    override def getMessage = {
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
  case class VariableNotDeclared(x:Name) extends AcumenError {
    override def getMessage =
      "Variable " + pprint(x) + " is not declared."
  }
  case class AccessDenied[A](id:A,self:A,children:List[A]) extends AcumenError {
    override def getMessage = 
      "Object #"+id+" is not self (i.e. #"+self+") nor a child of self" + 
         (if (children.length > 0) " (i.e. "+ children.map("#"+_).mkString(", ")+")"
          else "") + "."
  }
  case class NotAChildOf[A](id:A, obj:A) extends AcumenError {
    override def getMessage = 
      "Object #"+id+" is not a child of #"+obj+"."
  }
  case class NoMatch(gv:GroundValue) extends AcumenError {
    override def getMessage = 
      "No case matching " + pprint(gv) + "."
  }
  case class BadLhs() extends AcumenError {
    override def getMessage = 
      "The left hand-side of an assignment must be of the form 'e.x'."
  }
  case class BadPreLhs() extends AcumenError {
    override def getMessage = 
      "The left hand-side of an equation must be a field."
  }
  case class BadMove() extends AcumenError {
    override def getMessage = 
      "Move statements must have the form 'move o1.x o2'."
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

  case class BadProgramOptions(valid:Seq[String]) extends AcumenError {
    override def getMessage =
      "Bad command-line options. Valid options are " + valid.mkString(", ") + "."
  }

  case class UnrecognizedInterpreterString(theString: String) extends AcumenError {
    override def getMessage = 
      "Unrecognized interpreter string: " + theString
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
}
