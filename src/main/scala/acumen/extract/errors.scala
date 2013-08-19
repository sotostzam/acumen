package acumen
package extract

import Pretty._

/***************************************************************************
 * Exception used for error reporting.
 ***************************************************************************/

case class UnhandledSyntax[T](syntax: T, reason: String)(implicit prettyAble:PrettyAble[T]) extends Errors.AcumenError {
  override def getMessage = 
    "H.A. Extraction: Unhandled Syntax: " + pprint(syntax)
}
case class OtherUnsupported(msg: String) extends Errors.AcumenError {
  override def getMessage = "H.A. Extraction: " + msg
}

