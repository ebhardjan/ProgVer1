package core

import org.scalatest.FunSuite
import util.Smt2FileUtils

/**
  * Created by Severin on 2017-03-09.
  *
  * Test correctness of DPLL algorithm with hand-made formulas.
  */
class DPLLSolverTest extends FunSuite {
  val folder = "src/test/resources/solving"

  /**
    * Create as many tests as there are files in the specified folder.
    */
  for (f <- TestUtils.getListOfSmt2Files(folder)) {
    test("dpll_solver_" + f) {
      val formula = Smt2FileUtils.readSmt2File(folder, f.split(".smt2")(0))
      assert(SolverValidator.solveFormulaAndValidate(formula, new DPLLSolver))
    }
  }

  /**
    * for debugging...
    */
  test("dummy_test") {
    // paste number of failing test here to debug manually
    val testNr = "05"

    val formula = Smt2FileUtils.readSmt2File(folder, "test" + testNr)
    assert(SolverValidator.solveFormulaAndValidate(formula, new DPLLSolver))
  }
}
