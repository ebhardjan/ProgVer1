package core

import java.io.{File, PrintWriter}

import smtlib.parser.Commands._
import smtlib.parser.Terms.Term
import util.PropositionalLogic

import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by Severin on 2017-03-16.
  *
  * Run the test from a folder with all three algorithms, measuring the runtime. Store all the gathered data in a file.
  */
object AlgorithmEvaluator {
  val srcFolder: String = "src/test/resources/solving/"
  val targetFolder: String = "src/test/resources/solving"

  // Specify number of runs over which the runtime should be averaged.
  val nRuns: Int = 1
  // Specify maximum time to let the algorithm run before giving up and specifying runtime as 'i'.
  val maxRuntime: FiniteDuration = 1 second

  private[this] def getListOfSmt2Files(directoryPath: String): List[String] = {
    val directory = new File(directoryPath)
    if (directory.exists && directory.isDirectory) {
      directory.listFiles
        .filter(f => f.isFile)
        .filter(f => f.getName.contains(".smt2") || f.getName.contains(".cnf"))
        .map(f => f.getName)
        .toList
    } else {
      throw new IllegalStateException("The folder " + directoryPath + " does not exist.")
    }
  }

  private def readFormulaFile(folder: String, filename: String): Term = {
    val path: String = folder + "/" + filename
    val inputString: String = {
      if(filename.contains(".cnf")) {
        MySATSolver.convertDIMACSFileToSMTLIBv2(path)
      } else {
        val source = scala.io.Source.fromFile(path)
        try source.mkString finally source.close() // make sure the file is closed
      }
    }
    val script: List[Command] = MySATSolver.parseInputString(inputString)

    val (_, formula) = util.InputProcessing.processCommands(script)
    formula
  }

  private def runAlgorithm(formula: Term, solver: SATSolvingAlgorithm): String = {
    try {
      Await.result(Future[String] {
        try {
          val t0 = System.currentTimeMillis()
          solver.checkSAT(formula)
          val t1 = System.currentTimeMillis()
          (t1 - t0).toString
        } catch {
          case _: OutOfMemoryError =>
            "x"
        }
      }, maxRuntime)
    } catch {
      case _: TimeoutException => "i"
    }
  }

  private def averagedAlgorithmRuns(formula: Term, solver: SATSolvingAlgorithm, n: Int): String = {
    var results: Seq[String] = Seq()
    for (_ <- 1 to n) {
      val nextResult = runAlgorithm(formula, solver)
      nextResult match {
        case "i" => return "i"
        case "x" => return "x"
        case _ => results = nextResult +: results
      }
    }
    (results.foldLeft[Double](0)((s, next) => {
      s + next.toDouble
    }) / n).toString
  }

  def runExperiments(): String = {
    val filename: String = targetFolder + "/evaluations.txt"
    val pw = new PrintWriter(new File(filename))

    pw.write(s"nRuns=$nRuns i=${maxRuntime.toString()}\n")

    for (f <- getListOfSmt2Files(srcFolder)) {
      println(s"Parsing file $f")
      val formula = readFormulaFile(srcFolder, f)
      val cnf = {
        if (PropositionalLogic.isCNF(formula)) {
          formula
        } else {
          CNFConversion.toCNF(formula)
        }
      }

      val dpFuture: Future[String] = Future {
        println(s"Running DP on $f")
        averagedAlgorithmRuns(cnf, DPSolverWrapper, nRuns)
      }
      // use this to make execution sequential
      Await.result(dpFuture, Duration.Inf)

      val dpllFuture: Future[String] = Future {
        println(s"Running DPLL on $f")
        averagedAlgorithmRuns(cnf, new DPLLSolver, nRuns)
      }

      val cdclFuture: Future[String] = Future {
        println(s"Running CDCL on $f")
        averagedAlgorithmRuns(cnf, CDCLSolverWrapper, nRuns)
      }

      val currentLine = for {
        dpResult <- dpFuture
        dpllResult <- dpllFuture
        cdclResult <- cdclFuture
      } yield pw.write("dp:" + dpResult + "  dpll:" + dpllResult + "  cdcl:" + cdclResult + s" ($f)\n")

      Await.result(currentLine, Duration.Inf)
    }
    pw.close()
    filename
  }
}
