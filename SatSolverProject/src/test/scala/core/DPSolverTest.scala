package core

import java.io.File

import org.scalatest.FunSuite

/**
  * Created by Severin on 2017-03-07.
  *
  * Test correctness of DP algorithm with hand-made formulas.
  */
class DPSolverTest extends FunSuite {
  val folder = "src/test/resources/solving"

  private[this] def getListOfSmt2Files(directoryPath: String): List[String] = {
    val directory = new File(directoryPath)
    if (directory.exists && directory.isDirectory) {
      directory.listFiles
        .filter(f => f.isFile)
        .filter(f => f.getName.contains(".smt2"))
        .map(f => f.getName)
        .toList
    } else {
      throw new IllegalStateException("The folder " + directoryPath + " does not exist.")
    }
  }

  /**
    * Create as many tests as there are files in the specified folder.
    */
  for (f <- getListOfSmt2Files(folder)) {
    test("dp_solver_" + f) {
      val formula = CNFConversionTestUtils.readSmt2File(folder, f.split(".smt2")(0))
      assert(SolverValidator.solveFormulaAndValidate(formula, DPSolver))
    }
  }

  /**
    * for debugging...
    */
  test("dummy_test") {
    // paste number of failing test here to debug manually
    val testNr = "09"

    val formula = CNFConversionTestUtils.readSmt2File(folder, "test" + testNr)
    assert(SolverValidator.solveFormulaAndValidate(formula, DPSolver))
  }
}
