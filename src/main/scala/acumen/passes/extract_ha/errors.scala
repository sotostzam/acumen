package acumen
package passes
package extract_ha

import Pretty._

/***************************************************************************
 * Exception used for error reporting.
 ***************************************************************************/

case class UnhandledSyntax[T](syntax: T, reason: String = "Unknown")(implicit prettyAble:PrettyAble[T]) extends Errors.AcumenError {
  override def getMessage = 
    "H.A. Extraction: Unhandled Syntax: " + pprint(syntax)
}
case class OtherUnsupported(msg: String) extends Errors.AcumenError {
  override def getMessage = "H.A. Extraction: " + msg
}

