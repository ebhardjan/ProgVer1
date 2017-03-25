package core

import org.scalatest.FunSuite
import util.Smt2FileUtils

/**
  * Created by jan on 09.03.17.
  *
  * Formulas that previously failed with the CDCLSolverRandomTest
  */
class CDCLSolverPreviouslyFailingTest extends FunSuite {

  val folder = "src/test/resources/cdcl_solver/previously_failing"

  /**
    * Create as many tests as there are files in the specified folder.
    */
  for (f <- TestUtils.getListOfSmt2Files(folder)) {
    test("cdcl_solver_" + f) {
      val formula = Smt2FileUtils.readSmt2File(folder, f.split(".smt2")(0))
      assert(SolverValidator.solveFormulaAndValidate(formula, new CDCLSolver))
    }
  }

  /**
    * for debugging...
    */
  ignore("dummy_test") {
    // paste number of failing test here to debug manually
    val testNr = "277ea7b1-d67c-40a7-a09d-129bce3fb293"

    val formula = Smt2FileUtils.readSmt2File(folder, testNr)
    assert(SolverValidator.solveFormulaAndValidate(formula, new CDCLSolver))
  }
}
