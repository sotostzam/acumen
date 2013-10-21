package acumen

object Constants
{
  val PI = GDouble(math.Pi)

  def color(red: Double, green: Double, blue: Double) 
    = ExprVector(List(Lit(GDouble(red)), Lit(GDouble(green)), Lit(GDouble(blue))))

  val predefined : Map[String,Expr] = Map(
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
