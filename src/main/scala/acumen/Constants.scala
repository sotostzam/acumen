package acumen
import spire.math.Rational

object Constants
{
  // this is a separate variable so it can be reliably tested for
  // equality
  val PI = Lit(GRational(math.Pi))
  val rationalone = Lit(GRational(1))
  val rationalmone = Lit(GRational(-1))
  val rationalzero = Lit(GRational(0))
  def color(red: Rational, green: Rational, blue: Rational) 
    = ExprVector(List(Lit(GRational(red)), Lit(GRational(green)), Lit(GRational(blue))))

  val predefined : Map[String,Expr] = Map(
    // mathematical constants
    "pi" -> PI,
    // colors
    "red" -> color(1,0,0),
    "green" -> color(0,1,0),
    "blue" -> color(0,0,1),
    "white" -> color(1,1,1),
    "black" -> color(0,0,0),
    "yellow" -> color(1,1,0),
    "cyan" -> color(0,1,1),
    "magenta" -> color(1,0,1)
  )
}
