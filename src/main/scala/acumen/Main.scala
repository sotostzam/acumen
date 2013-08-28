package acumen

import java.io._

import Errors._
import util.Filters._
import render.ToPython._
import render.Java3D
import Pretty._

import com.sun.j3d.utils.applet.MainFrame

import java.net.{Socket, InetAddress, ServerSocket}

import scala.collection.mutable.ArrayBuffer

object ThreeDState extends Enumeration {
  val ERROR,DISABLE,LAZY,ENABLE = Value
}

object Main {

  //
  // What should be in Main
  //

  var need_quartz = false
  var threeDState: ThreeDState.Value = null
  var disableNewPlot = true
  var autoPlay = false
  var openFile: File = null
  var interpreter : Interpreter = null
  var useCompletion = true
  var useTemplates = false
  var dontFork = false
  var synchEditorWithBrowser = true // Synchronize code editor with file browser
  var extractHA = false
  var displayHelp = "none"

  var positionalArgs = new ArrayBuffer[String]

  // Note: lots of options and commands have alias but in general only
  //  the preferred version should be showed.  Lots also have
  //  enable|disable|no version but only one that changes the default
  //  behavior should be displayed
  def optsHelp = Array(
    "--help                  this help message",
    "-i|--semantics "+ Main.interpreterHelpString,
    "                        select interpreter to use",
    "--model <file>          model file to open",
    "--3d|--lazy-3d|--no-3d  controls the default state for the 3d tab",
    "--newplot               enable experimental plotter",
    "--play                  automatically run the model",
    "--disable-completion    disable code completion in the source code editor",
    "--dont-fork             disable auto-forking of a new JVM when required")
  def experimentalOptsHelp = Array(
    "--full-help",
    "--extract-ha",
    "--templates             enables template expansion in the source code editor."
  )
  def commandHelp = Array(
    "ui [<file>]             starts the U.I."
  )
  def experimentalCommandHelp = Array(
    "pretty <file>           pretty print model",
    "desugar <file>          pretty print desuguared model",
    "last <file>             run model and print final result",
    "trace <file>            run model and print trace output",
    "time <file>             time time it takes to run model",
    "",
    "extract <file>          extract H.A. and print result",
    "compile <file>          compile model to C++",
    "typecheck <file>        run type checker",
    "",
    "bench <file> <start> <stop> [<warmup> [<repeat>]]",
    "                        parallel benchmark",
    "bench-gnuplot ...       like \"bench\" but output gnuplot script",
    "bench-enclosures <file> <prefix> <repeat> [parms]",
    "                        enclosure benchmark", //  see --help-bench-enclosures",
    "listen <file> <portnum>",
    "",
    "examples                record reference outputs for test suite"
    //"2d|3d|java2d|java3d" // have no clue what these do! -- kevina
    //"fromJson",
  )

  def usage() = {
    System.err.println("Try --help for more information.")
    System.exit(1)
  }

  def parseArgs(args: List[String]) : Unit = try {
    args match {
      case Nil =>
      case ("--help" ) :: tail =>
        displayHelp = "normal"; parseArgs(tail)
      case ("--help-full"|"--full-help") :: tail =>
        displayHelp = "full"; parseArgs(tail)
      case ("--help-bench-enclosures") :: tail =>
        displayHelp = "bench-enclosures"; parseArgs(tail)
      case ("--semantics"|"--interpreter"|"-i") :: i :: tail =>
        interpreter = selectInterpreter(i); parseArgs(tail)
      case ("--model") :: f :: tail =>
        openFile = checkFile(f); parseArgs(tail)
      case ("--enable-3d" | "--3d") :: tail => 
        threeDState = ThreeDState.ENABLE; parseArgs(tail)
      case ("--lazy-3d") :: tail => 
        threeDState = ThreeDState.LAZY; parseArgs(tail)
      case ("--disable-3d" | "--no-3d") :: tail => 
        need_quartz = false
        threeDState = ThreeDState.DISABLE; 
        parseArgs(tail)
      case ("--enable-newplot" | "--newplot") :: tail => 
        disableNewPlot = false; parseArgs(tail)
      case ("--disable-newplot" | "--no-newplot") :: tail => 
        disableNewPlot = true; parseArgs(tail)
      case "--play" :: tail =>
        autoPlay = true; parseArgs(tail)
      case "--enable-completion" :: tail =>
        useCompletion = true; parseArgs(tail)
      case "--disable-completion" :: tail =>
        useCompletion = false; parseArgs(tail)
      case "--templates" :: tail =>
        useTemplates = true; parseArgs(tail)
      case "--dont-fork" :: tail =>
        dontFork = true; parseArgs(tail)
      case "--extract-ha" :: tail =>
        extractHA = true; parseArgs(tail)
      case opt ::  tail if opt.startsWith("-") =>
        System.err.println("Unrecognized Option: " + opt)
        usage()
      case arg :: tail => 
        positionalArgs += arg
        parseArgs(tail)
    }
  } catch {
    case e : AcumenError => 
      System.err.println(e.getMessage())
      usage()
  }

  def checkFile(fn: String) = {
    val openFile = new File(fn)
    if (!openFile.exists) {
      System.err.println("File not found: " + openFile)
      exit(1)
    }
    openFile
  }

  def selectInterpreter(args0: String*) : Interpreter = {
    val args = args0.flatMap(_.split('-')).toList
    import interpreters._
    val res = args match {
      case ("" | "reference") :: Nil => reference.Interpreter
      case "newreference" :: Nil => newreference.Interpreter
      case "parallel" :: tail => selectParallellInterpreter(tail)
      case "imperative" :: Nil => imperative.ImperativeInterpreter
      case "enclosure" :: tail => selectEnclosureInterpreter(tail)
      case _ => null
    }
    if (res == null) 
      throw UnrecognizedInterpreterString(args.mkString("-"))
      res
  }
  def interpreterHelpString = "reference|newreference|parallel[-<num threads>]|enclosure[-pwl|-evt]"
  // parallel-sharing should not be documented but recognized for testing

  def selectParallellInterpreter(args: List[String], 
                                 numThreads: Int = -1, 
                                 scheduler: String = "static") : Interpreter =
  {
    import interpreters.imperative.ParallelInterpreter._
    args match {
      case ("static"|"sharing") :: tail => selectParallellInterpreter(tail, numThreads, args(0))
      case head :: tail if head.matches("\\d+") => selectParallellInterpreter(tail, Integer.parseInt(head), scheduler)
      case Nil => (numThreads, scheduler) match {
        case (-1, "static") => static
        case (_, "static") => static(numThreads)
        case (-1, "sharing") => sharing
        case (_, "sharing") => sharing(numThreads)
      }
      case _ => null
    }
  }

  def selectEnclosureInterpreter(args: List[String], 
                                 eventHandler: String = "pwl") : Interpreter = {
    import interpreters.enclosure.Interpreter._
    args match {
      case ("pwl"|"evt") :: tail => selectEnclosureInterpreter(tail, args(0))
      case Nil => eventHandler match {
        case "pwl" => asPWL
        case "evt" => asEVT
      }
      case _ => null
    }
  }

  def main(args: Array[String]) : Unit = {
    parseArgs(args.toList)
    if (interpreter == null)
      interpreter = selectInterpreter("")
    if (displayHelp == "normal") {
      println("Options: ");
      optsHelp.foreach{line => println("  " + line)}
    } else if (displayHelp == "full") {
      println("Usage: acumen [options] [command]")
      println("");
      println("Options: ");
      optsHelp.foreach{line => println("  " + line)}
      println("");
      println("Experimental options:")
      experimentalOptsHelp.foreach{line => println("  " + line)}
      println("");
      println("Commands: ");
      commandHelp.foreach{line => println("  " + line)} 
      println("");
      println("Experimental commands:");
      experimentalCommandHelp.foreach{line => println("  " + line)}
    } else {
      (if (positionalArgs.size == 0) "ui" else positionalArgs(0)) match {
        case "ui" => 
          ui.GraphicalMain.main(args)
        case "examples"|"record-reference-outputs" => 
          examples
        case _ =>
          origMain(positionalArgs.toArray)
      }
    }
  }

  //
  // Other stuff that should eventually be factored out
  //

  var portNo : Int = 9999
  var serverSocket : ServerSocket = null
  var serverMode : Boolean = false
  var serverBufferedReader : BufferedReader = null
  var serverBufferedWriter : BufferedWriter = null

  def send_recv(s: String) : String = {
    serverBufferedWriter.write(s)
    serverBufferedWriter.newLine()
    serverBufferedWriter.flush()
    serverBufferedReader.readLine()
  }

  def as_ctrace(trace: InterpreterRes) = {
    trace match {case CStoreRes(r) => r; case _ => null}    
  }

  def origMain(args: Array[String]) : Unit = {
    try {
      val i = interpreter

      /* Read the Acumen source, parse, pre-process and interpret it. */
      lazy val in = new InputStreamReader(new FileInputStream(args(1)))
      lazy val ast = Parser.run(Parser.prog, in)
      lazy val desugared = Desugarer.run(ast)
      lazy val extracted = if (extractHA) new extract.Extract(desugared).res else desugared
      lazy val final_out = extracted // final output after all passes
      lazy val trace = i.run(final_out)
      lazy val ctrace = as_ctrace(trace)
      /* Perform user-selected action. */
      args(0) match {
        case "compile" => 
          val typeChecker = new TypeCheck(desugared)
          val res = typeChecker.run()
          interpreters.compiler.Interpreter.compile(desugared, typeChecker)
        case "pretty" => println(pprint(ast))
        case "desugar" => println(pprint(desugared))
        case "extract" =>
          val extr = new extract.Extract(desugared)
          println(pprint(extr.res))
        case "typecheck" => 
          val (typechecked, res) = new TypeCheck(desugared).run()
          println("\nTYPE CHECK RESULT: " + TypeCheck.errorLevelStr(res) + "\n")
          Pretty.withType = true
          println(pprint(typechecked))
        case "3d" => toPython3D(toSummary3D(ctrace))
        case "2d" => toPython2D(toSummary2D(ctrace))
        case "java2d" => new MainFrame(new Java3D(addThirdDimension(ctrace)), 256, 256);
        case "java3d" => new MainFrame(new Java3D(ctrace), 256, 256);
        case "json" => for (st <- ctrace) println(JSon.toJSON(st))
        case "fromJson" =>
          val st = ctrace(0)
          val x = JSon.fromJSON(JSon.toJSON(st).toString)
          println(x)
        case "listen" =>
          println("Model: " + args(0))
          serverMode = true
          portNo = args(2).toInt
          serverSocket = new ServerSocket(portNo)
          println("Listening on port " + portNo)
          val socket = serverSocket.accept()
          serverBufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream))
          serverBufferedWriter = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))
          ctrace.size // Force evaluation of the lazy value
        case "last" =>
          trace.printLast
        case "time" => 
          val forced = final_out
          val startTime = System.currentTimeMillis()
          trace.printLast
          val endTime = System.currentTimeMillis()
          println("Time to run: " + (endTime - startTime)/1000.0)
        case "bench" =>
          val offset = 2;
          val start: Int = Integer.parseInt(args(offset + 0))
          val stop: Int = Integer.parseInt(args(offset + 1))
          val warmup : Int = if (args.size > offset + 2) Integer.parseInt(args(offset+2)) else 0
          val repeat : Int = if (args.size > offset + 3) Integer.parseInt(args(offset+3)) else 10
          val forced = final_out
          for (nbThreads <- start to stop) {
        	interpreters.imperative.ParallelInterpreter(nbThreads)
            print(nbThreads + " threads: ")
            as_ctrace(i.run(forced)).last
            for (_ <- 0 until warmup) { print("w"); as_ctrace(i.run(forced)).last }
            val startTime = System.currentTimeMillis()
            for (_ <- 0 until repeat) { print("."); as_ctrace(i.run(forced)).last }
            val endTime = System.currentTimeMillis()
            println(endTime - startTime)
          }
        // the first six lines are shared with the "bench" case and would ideally not be repeated
        case "bench-gnuplot" =>
          val offset = 2;
          val start: Int = Integer.parseInt(args(offset + 0))
          val stop: Int = Integer.parseInt(args(offset + 1))
          val warmup : Int = if (args.size > offset + 2) Integer.parseInt(args(offset+2)) else 0
          val repeat : Int = if (args.size > offset + 3) Integer.parseInt(args(offset+3)) else 10
          val forced = final_out
          var data = Map[Int,Double]()
          for (nbThreads <- start to stop) {
            interpreters.imperative.ParallelInterpreter(nbThreads)
            as_ctrace(i.run(forced)).last
            for (_ <- 0 until warmup) { as_ctrace(i.run(forced)).last }
            val startTime = System.currentTimeMillis()
            for (_ <- 0 until repeat) { as_ctrace(i.run(forced)).last }
            val endTime = System.currentTimeMillis()
            val time = endTime - startTime
            System.err.print(".")
            data += nbThreads -> time 
          }
          // the result would ideally be written to a file, which would allow for 
          // outputting progress information as is done in the "bench" case
          println(Gnuplot.script(data))
        case "bench-enclosures" => 
          BenchEnclosures.run(i, final_out, args, 2)
        case "trace" =>
          trace.print
        case what =>
          System.err.println("Unrecognized Command: " + what)
          usage()
      }
    } catch {
      case e: AcumenError =>
        System.err.println(e.getMessage)
        System.exit(1)
      case e => throw e
    }
  }

  def examples = {
    var somethingUpdated = false
    Examples.cstoreExamplesAction{(dn, f) =>
      val loc = Examples.expectLoc
      val resFile = Examples.resultFile(loc, dn, f)
      if (resFile.exists) {
        println("skipping " + f + "")
      } else {
        somethingUpdated = true
        try {
          Examples.writeExampleResult(loc, dn, f, interpreters.reference.Interpreter)
        } catch {
          case e => 
            println("ERROR when processing " + f + ":")
            println("  " + e)
        }
        println("PROCESSED " + f)
      }
    }
    if (somethingUpdated) {
      println("Results updated.  Be sure to git add & commit the updated files as appropriate.")
    }
  }
}
