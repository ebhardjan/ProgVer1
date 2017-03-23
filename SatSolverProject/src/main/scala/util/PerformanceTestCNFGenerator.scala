package util

import smtlib.parser.Terms.Term

import scala.sys.process._

/**
  * Created by jan on 23.03.17.
  *
  * Generates the CNF formulas that are used in the performance tests.
  */
object PerformanceTestCNFGenerator {

  val debug: Boolean = false


  /**
    * usage:
    * - arg0 list of numVars with format: [3,5,10] (spaces are ignored)
    * - arg1 the directory where the smt2 files will be stored
    * - (arg2) optional argument to specify the number of formulas per variable count, default is 20
    * - (arg3) optional argument to specify the number of formulas per clause, default is 3
    *
    */
  def main(args: Array[String]) {

    if (args.length < 2 ||
      !(args(0).startsWith("[") && args(0).endsWith("]")) ||
      !args(1).endsWith("/")) {
      throw new IllegalArgumentException("usage: ... [3,5,10] relative/path/to/output/directory/")
    }

    val numVars: Seq[Int] = args(0).replace(" ", "")
      .substring(1, args(0).length - 1)
      .split(",")
      .map(s => Integer.decode(s).toInt)

    val directory = args(1)
    var numFormulas = 0
    var xSat = 0

    try {
      numFormulas = getIntArgOrDefault(args, argPos = 2, default = 20)
      xSat = getIntArgOrDefault(args, argPos = 3, default = 3)
    } catch {
      case e: NumberFormatException =>
        e.printStackTrace()
        throw new IllegalArgumentException("usage: ... [3,5,10] relative/path/to/output/directory/")
    }

    generateFormulas(numVars, directory, numFormulas, xSat)
  }

  private def getIntArgOrDefault(args: Array[String], argPos: Int, default: Int): Int = {
    if (args.length > argPos) {
      Integer.decode(args(argPos)).toInt
    } else {
      default
    }
  }

  def generateFormulas(numVars: Seq[Int], directory: String, numFormulas: Int, xSat: Int): Unit = {
    for (n <- numVars) {
      println(f"starting to look for a good number of clauses for $xSat-sat formulas with $n variables...")
      val numClauses = learnNumberOfClausesParameter(n, xSat)
      val generator = new RandomCNFGenerator(n, numClauses, xSat, xSat)
      var satCount: Int = 0
      var unSatCount: Int = 0

      while (satCount < numFormulas / 2 || unSatCount < numFormulas / 2) {
        val f = generator.generateRandomFormula()
        val isSat = sat(f)
        if ((isSat && satCount >= numFormulas / 2) || (!isSat && unSatCount >= numFormulas / 2)) {
          // only write the formula if we don't have enough formulas yet
        } else {

          if (isSat) {
            satCount += 1
          } else {
            unSatCount += 1
          }

          val satString: String = if (isSat) "sat" else "unsat"
          val count = if (isSat) satCount else unSatCount
          val filename = f"random_${xSat}sat_numVars_$n%03d_numClauses_$numClauses%04d_${satString}_$count%02d.smt2"

          Smt2FileUtils.writeFormulaToSmt2File(f, directory + filename, getModel = true)
        }
      }
      println(f"Wrote ${satCount + unSatCount} formulas to files ($satCount sat, $unSatCount unsat).")
      println("")
    }

  }

  private def learnNumberOfClausesParameter(numVars: Int, xSat: Int): Int = {
    // start value of the parameter search
    var nClauses = numVars * 2

    var changeSearchDirectionCount = 0
    // flag to stop the loop
    var continue = true
    while (continue && changeSearchDirectionCount < 2) {
      val satProb = calcSatProbability(numVars, xSat, nClauses)
      if (satProb >= 0.9) {
        // we are far away from the parameter we are looking for...
        nClauses += numVars
      }
      else if (satProb < 0.9 && satProb >= 0.56) {
        // we are close to the parameter we are looking for
        nClauses += 1
      }
      else if (Math.abs(satProb - 0.5) <= 0.03) {
        // parameter found, stop loop
        continue = false
      } else {
        // we went too far, go back
        nClauses -= 1
        changeSearchDirectionCount += 1
      }
    }
    println("found good parameter choice: numClauses:" + nClauses)
    nClauses
  }

  private def calcSatProbability(numVars: Int, xSat: Int, nClauses: Int): Double = {
    // number of formulas used to calculate the probability
    val numFormulas = 100

    val generator = new RandomCNFGenerator(numVars, nClauses, xSat, xSat)
    var satCount = 0

    for (_ <- 0 to numFormulas) {
      val f = generator.generateRandomFormula()
      if (sat(f)) {
        satCount += 1
      }
    }

    val satP = satCount.toDouble / numFormulas
    if (debug) {
      println(f"nClauses:$nClauses sat:$satP unsat:${1 - satP}")
    }
    satP
  }

  private def sat(formula: Term): Boolean = {
    val filename = "tmp.smt2"
    Smt2FileUtils.writeFormulaToSmt2File(formula, filename, getModel = false)
    val res = "z3 -smt2 " + filename !!
    val r = !res.contains("unsat")
    r
  }

}
