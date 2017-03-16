package core

import java.io.{File, PrintWriter}

import smtlib.parser.Commands._
import smtlib.parser.Terms.Term


/**
  * Created by Severin on 2017-03-16.
  *
  * Run the test from a folder with all three algorithms, measuring the runtime. Store all the gathered data in a file.
  */
object AlgorithmEvaluator {
  val srcFolder: String = "src/test/resources/solving"
  val targetFolder: String = "src/test/resources/evaluation"

  // Specify number of runs over which the runtime should be averaged.
  val nRuns: Int = 5

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
    val t0 = System.currentTimeMillis()
    try {
      solver.checkSAT(formula)
      val t1 = System.currentTimeMillis()
      (t1 - t0).toString
    } catch {
      case e: Exception => "x"
    }
  }

  private def averagedAlgorithmRuns(formula: Term, solver: SATSolvingAlgorithm, n: Int): String = {
    var results: Seq[String] = Seq()
    for (_ <- 1 to n) {
      results = runAlgorithm(formula, solver) +: results
    }
    results.foldLeft[String]("")((s, next) => {
      s match {
        case "x" => "x"
        case "" => next
        case n1 =>
          next match {
            case "x" => "x"
            case n2  => ((n2.toDouble + n1.toDouble) / 2).toString
          }
      }
    })
  }

  def runExperiments(): String = {
    val filename: String = targetFolder + "/evaluations.txt"
    val pw = new PrintWriter(new File(filename))

    val dpSolver: DPSolver = new DPSolver
    val dpllSolver: DPLLSolver = new DPLLSolver

    for (f <- getListOfSmt2Files(srcFolder)) {
      val formula = readFormulaFile(srcFolder, f)
      val cnf = CNFConversion.toCNF(formula)
      val currentLine: String = "dp:" + averagedAlgorithmRuns(cnf, dpSolver, nRuns) +
        "  dpll:" + averagedAlgorithmRuns(cnf, dpllSolver, nRuns) + "\n"
      pw.write(currentLine)
    }
    pw.close()
    filename
  }
}
