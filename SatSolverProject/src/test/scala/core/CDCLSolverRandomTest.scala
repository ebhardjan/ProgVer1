package core

import org.scalatest.FunSuite

/**
  * Created by jan on 13.03.17.
  *
  * Tests correctness of CDCL solver algorithm.
  *
  * Creates new random formulas and tests them, formulas that don't pass the test get stored and will be tested later
  * again by the CDCLSolverPreviouslyFailingTests test.
  */
class CDCLSolverRandomTest extends FunSuite {

  val folder = "src/test/resources/cdcl_solver/previously_failing/"
  val numberOfRandomFormulas = 50
  val storeFailedFormulas = true

  // random formula generator parameters
  val numberOfVariables = 50
  val maxChildCount = 25
  val minDepth = 4
  val maxDepth = 8

  val generator = new RandomFormulaGenerator(numberOfVariables, maxChildCount, minDepth, maxDepth)
  for (i <- 1 to numberOfRandomFormulas) {
    test("newRandomFormula_" + i) {
      val formula = CNFConversion.toCNF(generator.generateRandomFormula())
      val correct = SolverValidator.solveFormulaAndValidate(formula, new CDCLSolver)
      if (!correct && storeFailedFormulas) {
        // write formula to file
        CNFConversionTestUtils.writeFormulaToSmt2File(formula,
          folder + java.util.UUID.randomUUID.toString + ".smt2", getModel = true)
      }
      assert(correct)
    }
  }

}
