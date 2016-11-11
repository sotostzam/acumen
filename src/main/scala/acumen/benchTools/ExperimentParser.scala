/*
 * This fiel contains the parser for the input files used by the tool
 */

package benchTool

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.matching.Regex

/**
 * Define different types of execution
 */
sealed abstract class ExecutionType
// every models once, and repaet this pattern
case class Interleaved(nbIte: Int, time: Double) extends ExecutionType
// all the runs of the same model then the others
case class Single(nbIte: Int, time: Double) extends ExecutionType

sealed abstract class Experiment
case class ParsedExperimentType(exec: List[ExecutionType], inters:  List[String], mods:  List[String], opts : (Int, Int, Option[(Int, Double)])) extends Experiment


/**
 * Parser for the experiement files
 * extract models, semantics and way of running the experiments
 * used for the benchTool
 */
object ExperimentParser extends acumen.MyStdTokenParsers {
  /**
   * main parser method
   */
  def parse(s: String): ParsedExperimentType = {
    val res = phrase(file)(new lexical.Scanner(s))
    if (res.successful) res.get else throw new Exception(res.toString)
  }

  lexical.delimiters ++= List("{", "}", "(", ")", "<-", ",", "\"", "[", "/", "]", "=", "-", ".")
  lexical.reserved ++= List("acm", "executions", "Executions", "GNUPLOT", "interleaved", "Interleaved", "single", "Single", "models", "Models", "repeat",
    "seconds", "second", "semantics", "Semantics", "times", "warmup", "meta_test_max")

  //Parsers for keywords synonyms
  def kwSeconds: Parser[Unit] = ("second" | "seconds") ^^ { case _ => }
  def kwTimes: Parser[Unit] = ("times") ^^ { case _ => }
  def kwWarmup: Parser[Unit] = ("warmup") ^^ { case _ => }
  def kwRepeat: Parser[Unit] = ("repeat") ^^ { case _ => }
  def kwTableOption: Parser[Unit] = ("meta_test_max") ^^ { case _ => }
  def kwSemantics: Parser[Unit] = ("semantics" | "Semantics") ^^ { case _ => }
  def kwExecutions: Parser[Unit] = ("executions" | "Executions") ^^ { case _ => }
  def kwModels: Parser[Unit] = ("models" | "Models") ^^ { case _ => }

  def file: Parser[ParsedExperimentType] =
    executions ~ semantics ~ models ~ optionalParameter ^^ {
      case (exec ~ sem ~ mods ~ opt) => ParsedExperimentType(exec, sem, mods, opt)
    }

  // Execution perser : how many and how run the models
  def executions: Parser[List[ExecutionType]] = kwExecutions ~> "<-" ~> "{" ~> rep1sep(execution, ",") <~ "}"
  def execution: Parser[ExecutionType] =
    coupleTimesAndTime ~ ("interleaved" | "Interleaved" | "single" | "Single") ^^ {
      case (a, b) ~ way => way match {
        case "interleaved" | "Interleaved" => Interleaved(a, b)
        case "single" | "Single"           => Single(a, b)
      }
    }
  def coupleTimesAndTime: Parser[(Int, Double)] = numericLit ~ kwTimes ~ (numericLit|rationalLit) ~ kwSeconds ^^ {
    case nbIte ~ kw1 ~ time ~ kw2 => (nbIte.toInt, time.toDouble)
  }
    
  // List of names of interpreters to use
  def semantics: Parser[List[String]] = kwSemantics ~> "<-" ~> "{" ~> rep1sep(stringLit, ",") <~ "}"

  // Parsers for the models section
  def models: Parser[List[String]] = kwModels ~> "<-" ~> "{" ~> rep1sep(path, ",") <~ "}" ^^ {
    case a => a.flatten
  }
  def path: Parser[List[String]] = rep(dashedName <~ "/") ~ regWord ~ "." ~ "acm" ^^ {
    case list ~ reg ~ p ~ ext => for (w <- reg) yield list.mkString("/") + "/" + w + ".acm"
  }
  def regWord: Parser[List[String]] = opt(dashedName) ~ option ~ opt(regWord) ^^ {
    case None ~ optList ~ None         => optList
    case None ~ optList ~ Some(reg)    => for (o <- optList; r <- reg) yield (o + r)
    case Some(w) ~ optList ~ None      => optList.map((x) => w + x)
    case Some(w) ~ optList ~ Some(reg) => for (o <- optList; r <- reg) yield (w + o + r)
  } | dashedName ^^ (x => List(x)) // Order of thing matters : the match must start with the most complex 
  def option: Parser[List[String]] = "[" ~> rep1sep(dashedName, ",") <~ "]"
  def dashedName: Parser[String] = rep1sep(name, "-") ^^ { case a => a.mkString("-") }
  def name: Parser[String] = opt(numericLit) ~ ident ^^ {
    case None ~ b    => b.toString
    case Some(a) ~ b => a.toString + b.toString
  }

  def optionalParameter: Parser[(Int, Int, Option[(Int, Double)])] = optWarmup ~ optRepeat ~ opt(statisticsOptions) ^^ {
    case warm ~ rept ~ table => (warm, rept, table)
  }
  def optWarmup: Parser[Int] = opt(warmup) ^^ {
    case None    => 0
    case Some(a) => a.toInt
  }
  def warmup: Parser[String] = kwWarmup ~> "=" ~> numericLit
  def optRepeat: Parser[Int] = opt(repeat) ^^ {
    case None    => 0
    case Some(a) => a.toInt
  }
  def repeat: Parser[String] = kwRepeat ~> "=" ~> numericLit
  def statisticsOptions: Parser[(Int, Double)] = kwTableOption ~> "=" ~> "(" ~> coupleTimesAndTime <~ ")"

}
