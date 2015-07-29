package acumen

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import benchTool._

object ExperimentGenerator {
  val timeKwGen = value("times") :| "Failed to generate time-keyword"
  val warmupKwGen = value("warmup") :| "Failed to generate warmup-keyword"
  val repeatKwGen = value("repeat") :| "Failed to generate repeat-keyword"
  val tableOptionKwGen = value("meta_test_max") :| "Failed to generate tableOption-keyword"
  val secondsKwGen = oneOf("second", "seconds") :| "Failed to generate seconds-keyword"
  val executionsKwGen = oneOf("executions", "Executions") :| "Failed to generate executions-keyword"
  val interleavedWayKwGen = oneOf("interleaved", "Interleaved") :| "Failed to generate interleaved-keyword"
  val singleWayKwGen = oneOf("single", "Single") :| "Failed to generate single-keyword"
  val semanticsKwGen = oneOf("semantics", "Semantics") :| "Failed to generate semantics-keyword"
  val modelsKwGen = oneOf("models", "Models") :| "Failed to generate models-keyword"
  val coupleTimesAndTimeGen = for (
    m <- choose(0, 9);
    t <- timeKwGen;
    n <- choose(0.0, 9);
    s <- secondsKwGen
  ) yield m + " " + t + " " + n + " " + s
  val singleExecutionGen = for (
    values <- coupleTimesAndTimeGen;
    way <- singleWayKwGen
  ) yield values + " " + way
  val onlyOneSingleGen = for (
    kw <- executionsKwGen;
    exec <- singleExecutionGen
  ) yield kw + "<- {" + exec + "}"
  val interleavedExecutionGen = for (
    values <- coupleTimesAndTimeGen;
    way <- interleavedWayKwGen
  ) yield values + " " + way
  val executionGen = oneOf(singleExecutionGen, interleavedExecutionGen)
  val executionsGen = for (
    kw <- executionsKwGen;
    n <- choose(1, 5);
    seq <- listOfN(n, executionGen)
  ) yield kw + "<- {" + seq.mkString(", ") + "}"

  val semanticGen = oneOf(
    "reference2015",
    "optimized2015",
    "reference2014",
    "optimized2014",
    "reference2013",
    "optimized2013" //    "reference2012",
    //    "optimized2012",
    //    "parallel2012"
    )
  val semanticsGen = for (
    kw <- semanticsKwGen;
    n <- choose(1, 5);
    seq <- listOfN(n, semanticGen)
  ) yield kw + "<- {\"" + seq.mkString("\", \"") + "\"}"
  val modelGen = oneOf(
    "examples/04_Experimental/01_Enclosures/04_Zeno/07_Two_Tanks.acm",
    "examples/04_Experimental/02_Robust_Simulation/05_BB_validated_w_x_geq_0.acm",
    "examples/03_Projects/03_Anita_Santanna/10_TAMU_compassGait.acm",
    "examples/03_Projects/02_Jawad/01_ADAS_CIB_Rear_END.acm",
    "examples/05_Language_Research/23_HydLa/02_Bouncing_Ball_With_Hole-3D.acm",
    "examples/00_Demos/06_Multiple_examples_in_one_file.acm",
    "examples/04_Experimental/01_Enclosures/05_Parameters/06_minLocalizationStep_Small.acm",
    "examples/01_CPS_Course/04_Control_Theory/99_inverted_pendulum.acm ",
    "examples/03_Projects/04_Yingfu_Zeng/03_BreakingBall.acm")
  val modelsGen = for (
    kw <- modelsKwGen;
    n <- choose(1, 5);
    seq <- listOfN(n, modelGen)
  ) yield kw + "<- {" + seq.mkString(",") + "}"
  val warmupGen = for (
    kw <- warmupKwGen;
    n <- choose(0, 10)
  ) yield kw + " = " + n
  val repeatGen = for (
    kw <- repeatKwGen;
    n <- choose(0, 10)
  ) yield kw + " = " + n
  val tableOptionGen = for (
    kw <- tableOptionKwGen;
    values <- coupleTimesAndTimeGen
  ) yield kw + " = (" + values + ")"

  val singleExperimentFile = for (
    exec <- onlyOneSingleGen;
    sem <- semanticsGen;
    mods <- modelsGen;
    warm <- warmupGen;
    rep <- repeatGen;
    coin <- arbitrary[Boolean];
    tabOpt <- tableOptionGen
  ) yield exec + "\n" + sem + "\n" + mods + "\n" + warm + "\n" + rep + (if (coin) "\n" + tabOpt else "")
  val experimentFile = for (
    exec <- executionsGen;
    sem <- semanticsGen;
    mods <- modelsGen;
    warm <- warmupGen;
    rep <- repeatGen;
    coin <- arbitrary[Boolean];
    tabOpt <- tableOptionGen
  ) yield exec + "\n" + sem + "\n" + mods + "\n" + warm + "\n" + rep + (if (coin) "\n" + tabOpt else "")
}

object PrettyPrint {
  def printInput(output: ParsedExperimentType): String = {
    output match {
      case ParsedExperimentType(e, i, m, o) => {
        val execs = printExec(e)
        val inters = printInter(i)
        val models = printModel(m)
        val opts = printOptions(o)
        execs + "\n" + inters + "\n" + models + "\n" + opts
      }
    }
  }
  def printExec(list: List[ExecutionType]): String = {
    val execs = for (l <- list) yield l match {
      case Interleaved(a, b) => a + " times " + b + " seconds interleaved"
      case Single(a, b)      => a + " times " + b + " seconds single"
    }
    "executions <- {" + execs.mkString(",") + "}"
  }
  def printModel(list: List[String]): String = {
    "models <- {" + list.mkString(",") + "}"
  }
  def printInter(list: List[String]): String = {
    val formatedList = list.map(x => "\"" + x + "\"")
    "semantics <- {" + formatedList.mkString(",") + "}"
  }
  def printOptions(opt: (Int, Int, Option[(Int, Double)])): String = {
    opt match {
      case (warm, repeat, None)         => "\nwarmup = " + warm + "\nrepeat = " + repeat
      case (warm, repeat, Some((m, n))) => "warmup = " + warm + "\nrepeat = " + repeat + "\nmeta_test_max = ( " + m + " times " + n + " seconds )"
    }
  }

}

object BenchToolTest extends Properties("BenchTool") {

  property("parser") = forAll(ExperimentGenerator.experimentFile) {
    str =>
      //      println("\nSTART" + str)
      val first_parsed = ExperimentParser.parse(str)
      //      println("First parse succeed")
      val parsed = ExperimentParser.parse(str)
      val prettyString = PrettyPrint.printInput(parsed)
      //      println("pretty print succeed")
      val second_parsed = ExperimentParser.parse(prettyString)
      //      println("Second parse succeed " + "first parsed = \n" + first_parsed + "\n second = \n" + second_parsed)
      first_parsed.equals(second_parsed) :| "first parsed = " + first_parsed + "\n second = " + second_parsed
  }
  property("SingleGenerator") = forAll(ExperimentGenerator.singleExperimentFile) {
    str =>
      //      println("\nSTART" + str)
      val first_parsed = ExperimentParser.parse(str)
      //      println("First parse succeed")
      val parsed = ExperimentParser.parse(str)
      val prettyString = PrettyPrint.printInput(parsed)
      //      println("pretty print succeed")
      val second_parsed = ExperimentParser.parse(prettyString)
      //      println("Second parse succeed " + "first parsed = \n" + first_parsed + "\n second = \n" + second_parsed)
      first_parsed.equals(second_parsed) :| "first parsed = " + first_parsed + "\n second = " + second_parsed
  }

  //Fail for heap size reasons
  property("single") = forAll(ExperimentGenerator.singleExperimentFile) {
    str =>
      println(str)
      var ok_Invalid = true
      var ok_min = true
      var ok_lowDeviation = true
      var ok_upDeviation = true
      try {
        val ParsedExperimentType(exec, inters, mods, (warmup, rep, opt)) = ExperimentParser.parse(str)

        //      true}

        for (e <- exec) e match {
          case Single(nbIte, time) =>
            val result = mathTools.applyMinAndQuartiles(SingleRun.run(mods, inters, time, nbIte))
            println(result)
            for (res <- result) {
              val (q0, d1, d2) = res._3.head
              if (q0 < 0) ok_min = false
              if (d1 < 0) ok_lowDeviation = false
              if (d2 < 0) ok_upDeviation = false
            }
          case _ => ok_Invalid = false
        }
      } catch {
        case t: Throwable => t.printStackTrace(); true
      }
      all(
        "Invalid output from parser" |: ok_Invalid,
        "bottom deviation incorrect" |: ok_lowDeviation,
        "upper deviationin incorrect" |: ok_min,
        "negative time for minimum" |: ok_upDeviation)
  }

  //  property("interleaved") = forAll() {
  //    x => true
  //  }

}

