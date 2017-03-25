package core

import org.scalatest.FunSuite
import util.{RandomCNFGenerator, Smt2FileUtils}

/**
  * Created by jan on 23.03.17.
  *
  * Tests correctness of DPLL solver algorithm.
  *
  * Creates new random formulas and tests them, formulas that don't pass the test get stored and will be tested later
  * again by the DPPLSolverPreviouslyFailingTests test.
  */
class DPLLSolverRandomTest extends FunSuite {

  val folder = "src/test/resources/dpll_solver/previously_failing/"
  val numberOfRandomFormulas = 50
  val storeFailedFormulas = true

  // random formula generator parameters
  val numberOfVariables = 6
  val numberOfClauses = 25
  val minLiteralCount = 2
  val maxLiteralCount = 4

  val generator = new RandomCNFGenerator(numberOfVariables, numberOfClauses, minLiteralCount, maxLiteralCount)
  for (i <- 1 to numberOfRandomFormulas) {
    test("newRandomFormula_" + i) {
      val formula = CNFConversion.toCNF(generator.generateRandomFormula())
      var correct = false
      try {
        correct = SolverValidator.solveFormulaAndValidate(formula, new DPLLSolver)
      } catch {
        case _: Throwable => println("Exception occurred!")
      }
      if (!correct && storeFailedFormulas) {
        // write formula to file
        Smt2FileUtils.writeFormulaToSmt2File(formula,
          folder + java.util.UUID.randomUUID.toString + ".smt2", getModel = true)
      }
      assert(correct)
    }
  }

}
