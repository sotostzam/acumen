package acumen.util
import scala.actors.Futures._

// missing functions in scala std lib

object Collections {

  def any (l:TraversableOnce[Boolean]) : Boolean = l.foldLeft(false)(_||_)

  def intersperse[A](e:A)(xs:List[A]): List[A] = 
    xs match {
	  case Nil      => Nil
      case x :: Nil => x :: Nil
	  case x :: xs  => x :: e :: intersperse(e)(xs)
  }

}
