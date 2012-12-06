package acumen

import Errors._
import Pretty._
import interpreters.parallel.Interpreter._
import util.Filters._
import Ordering.Implicits._

object BenchEnclosures {

  val REPEAT = 3

  def run(i: Interpreter, prog: Prog, args: Array[String], argsOffset : Int) {
    val parms = getParms(args.slice(argsOffset, Int.MaxValue))
    var trials = flatten(cartesianProduct(parms))
    trials = duplicate(3, trials)
    trials = scala.util.Random.shuffle(trials)
    val ie = i.asInstanceOf[acumen.interpreters.enclosure.Interpreter]
    //val res = trials.map{el: List[(String, Double)] => 
    //  (el.map{(_._2)}.toArray, new Array[Double](3))}.toMap
    var res = collection.mutable.MutableList[(List[(String, Double)],Double)]()
    println("Prepping with default parms.")
    ie.run(prog)
    println("Prep done, time irrelevant")
    for (adjustments <- trials) {
      println("===")
      def adjustParms(p: acumen.interpreters.enclosure.Parameters) =
        adjustments.foldLeft(p){case (p,(k,v)) => k match {
          case "minTimeStep"    => p.copy(minTimeStep = v)
          case "maxTimeStep"    => p.copy(maxTimeStep = v)
          case "minImprovement" => p.copy(minImprovement = v)
          case what              => throw new Error("Unknown parm: " + what)
        }}
      println("Starting with parms: " + adjustments)
      val s = System.currentTimeMillis
      val r = ie.runInterpreter(prog,ie.defaultInterpreterCallbacks,adjustParms)
      r.printLast
      val time = (System.currentTimeMillis - s)/1000.0
      println("Time to run simulation: %f".format(time))
      res += ((adjustments, time))
    }
    val grouped = res.groupBy{_._1.map{_._2}.toSeq}.mapValues{_.map{_._2}.toArray.sorted}
    println("===")
    println("RESULTS")
    println(parms.map{_._1}.flatten.mkString(" ") + " : " +
            "avg sd : " +
            "raw_data_sorted " + Stream.fill(REPEAT-1)("-").toList.mkString(" "))
    for (adjustments <- grouped.keys.toList.sorted) {
      val vals = grouped(adjustments)
      val avg = vals.sum / vals.length
      val sd = math.sqrt(vals.map{x => math.pow(x - avg,2)}.sum / (vals.length - 1))
      println(adjustments.map{_.toString}.mkString(" ") + " : " + 
              avg + " +- " + sd + " : " +
              vals.map{_.toString}.mkString(" "))
    }
  }

  def getParms(args: Array[String]) : List[(Array[String], List[Array[Double]])] = 
    args.map{arg: String => val name :: vals :: nil = arg.split("=").toList
                            (name.split(":"), 
                             vals.split(",").toList.map{_.split(":").map{_.toDouble}})}
      .toList

  def cartesianProduct(parms: List[(Array[String], List[Array[Double]])]) : List[List[(Array[String], Array[Double])]] = {
    def step(ps: List[(Array[String], List[Array[Double]])]) : List[List[(Array[String], Array[Double])]] = ps match {
      case (key, vals) :: rest => product(vals.map{v => (key, v)}, step(rest))
      case what                => List(Nil)
    }
    def product(heads: List[(Array[String], Array[Double])], tails: List[List[(Array[String], Array[Double])]]) = {
      val res = for (head <- heads; tail <- tails) yield head :: tail
      res
    }
    step(parms)
  }

  def flatten(trials: List[List[(Array[String], Array[Double])]]) : List[List[(String, Double)]] =
    trials.map{adjs : List[(Array[String], Array[Double])] => 
      var res : List[(String, Double)] = Nil
      for ((key,v) <- adjs) {
        for (idx <- key.indices) {
          res = (key(idx), v(idx)) :: res
        }
      }
      res.reverse
    }
  
  def duplicate(times: Int, trials: List[List[(String, Double)]]) : List[List[(String, Double)]] = 
    if (times <= 1) trials
    else trials ++ duplicate(times -1, trials)

}
