
package acumen.interpreters.enclosure

// import Field

class Transcend (p:Parameters) extends Application {

// 1. Compute co-enc for co-sine

   // 1.1 Create a field

   val field = Field (Map("x" -> Variable ("y"), "y" -> -Variable ("x")));
   val mode  = Mode("mode");
   val hysys = HybridSystem (Set(mode),
                             Set[Event](),
                             field,
                             Map(),
                             Map());

   // 1.2 Solve it over [0,1.6]

   val inter = Interval (0,1.6) // Where we will compute the solution

   val icbox = Box ("x" -> Interval(1,1), "y" -> Interval (0,0));
   val icsta = new StateEnclosure (Map(mode -> Some(icbox)));

   val coenc = enclosePiecewise
                (p,
                 hysys, 
                 interval,
                 s: StateEnclosure, 
                 null)
                (Rounding(p.bigDecimalDigits));

// 2. Use co-enc

println(coenc);

}