package acumen

import scala.io._
import java.io.InputStreamReader
import java.io.FileInputStream

import Errors._
import util.Filters._
import interpreters.parallel.Interpreter._
import render.ToPython._
import render.Java3D
import Pretty._

import com.sun.j3d.utils.applet.MainFrame

import javax.swing._

object Main {
    
  def main(args : Array[String]) : Unit = {

    val I = interpreters.reference.Interpreter
    //val I = new interpreters.parallel.Interpreter(2)

    val in = new InputStreamReader(System.in)
    
  	lazy val ast        = Parser.run(Parser.prog, in)
  	lazy val diffed     = SD.run(ast)
    lazy val desugared  = Desugarer.run(diffed)
    lazy val dva_out    = DVA.run(desugared)
    lazy val bta_out    = BTA.run(dva_out)
    lazy val spec_out   = Specializer.run(bta_out)
    lazy val nodiff_out = AD.run(spec_out)
    lazy val trace      = I.run(nodiff_out)
    lazy val ctrace     = trace map I.repr

    try {
      args(0) match {
        case "pretty" => println(pprint(ast))
        case "desugar" => println(pprint(desugared))
        case "3d" => toPython3D(toSummary3D(ctrace))
        case "2d" => toPython2D(toSummary2D(ctrace))
        case "java2d" => new MainFrame(new Java3D(addThirdDimension(ctrace)), 256, 256);
        case "java3d" => new MainFrame(new Java3D(ctrace), 256, 256);
        case "json" => for (st <- ctrace) println(JSon.toJSON(st))
        case "last" => 
          println(pprint(prettyStore(I.repr(trace.last))))
        case "bench" => 
          val start : Int = Integer.parseInt(args(1))
          val stop  : Int = Integer.parseInt(args(2))
          val forced = nodiff_out
          for (nbThreads <- start to stop) {
            print(nbThreads + " threads: ")
            withInterpreter(nbThreads) { PI =>
               PI.run(forced).last 
               print(".")
               val startTime = System.currentTimeMillis()
               for (_ <- 0 until 10) { print("."); PI.run(forced).last }
               val endTime = System.currentTimeMillis()
               println(endTime - startTime)
            }
          }
        case "trace" =>
          var i = 0
          for (st <- ctrace) {
            println(pprint(prettyStore(st)))
            println("-"*30 + i)
            i += 1
          }
        case _ => 
          throw BadProgramOptions(
            List("pretty", "desugar", "3d", "2d", "java2d",
                 "java3d", "json", "last", "bench from to", "trace"))
      }
    } catch {
      case e:AcumenError => 
        System.err.println(e.getMessage)
        System.exit(1)
      case e => throw e
    }
  }
}
