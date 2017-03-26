package core

import org.scalatest.FunSuite
import util.Smt2FileUtils

/**
  * Created by Severin on 2017-03-07.
  *
  * Test correctness of DP algorithm with hand-made formulas.
  */
class DPSolverTest extends FunSuite {
  val folder = "src/test/resources/solving"

  /**
    * Create as many tests as there are files in the specified folder.
    */
  for (f <- TestUtils.getListOfSmt2Files(folder)) {
    test("dp_solver_" + f) {
      if (!f.equals("test12.smt2")) {
        val formula = Smt2FileUtils.readSmt2File(folder, f.split(".smt2")(0))
        assert(SolverValidator.solveFormulaAndValidate(formula, new DPSolver))
      }
    }
  }

  /**
    * for debugging...
    */
  ignore("dummy_test") {
    // paste number of failing test here to debug manually
    val testNr = "09"

    val formula = Smt2FileUtils.readSmt2File(folder, "test" + testNr)
    assert(SolverValidator.solveFormulaAndValidate(formula, new DPSolver))
  }
}
