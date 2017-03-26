package core

import smtlib.parser.Commands._
import smtlib.parser.Terms.Term
import util.PropositionalLogic

import scala.concurrent.{Await, Future, TimeoutException}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by Severin on 2017-03-16.
  *
  * Run all three SAT solving algorithms on a given file and measure runtime of each.
  */
object AlgorithmEvaluator {
  // Specify number of runs over which the runtime should be averaged.
  val nRuns: Int = 5
  // Specify maximum time to let the algorithm run before giving up and specifying runtime as 'i'.
  val maxRuntime: FiniteDuration = 20 seconds

  private def readFormulaFile(path: String): Term = {
    val inputString: String = {
      if(path.contains(".cnf")) {
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

  def runExperiments(formulaFile: String): String = {
    println(s"Parsing file $formulaFile")
    val formula = readFormulaFile(formulaFile)
    val cnf = {
      if (PropositionalLogic.isCNF(formula)) {
        formula
      } else {
        CNFConversion.toCNF(formula)
      }
    }

    println(s"nRuns=$nRuns i=${maxRuntime.toString()}")

    val dpFuture: Future[String] = Future {
//      println(s"Running DP on $formulaFile")
      averagedAlgorithmRuns(cnf, DPSolverWrapper, nRuns)
    }
    // use this to make execution sequential
    Await.result(dpFuture, Duration.Inf)

    val dpllFuture: Future[String] = Future {
//      println(s"Running DPLL on $formulaFile")
      averagedAlgorithmRuns(cnf, new DPLLSolver, nRuns)
    }
    Await.result(dpllFuture, Duration.Inf)
    val cdclFuture: Future[String] = Future {
//      println(s"Running CDCL on $formulaFile")
      averagedAlgorithmRuns(cnf, CDCLSolverWrapper, nRuns)
    }

    val currentLine = for {
      dpResult <- dpFuture
      dpllResult <- dpllFuture
      cdclResult <- cdclFuture
    } yield "dp:" + dpResult + "  dpll:" + dpllResult + "  cdcl:" + cdclResult + s" ($formulaFile)\n"

    Await.result[String](currentLine, Duration.Inf)
  }
}
