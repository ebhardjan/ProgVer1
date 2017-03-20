package core

import java.io.File

import org.scalatest.FunSuite

/**
  * Created by jan on 09.03.17.
  *
  * Test correctness of CDCL algorithm with hand-made formulas.
  */
class CDCLSolverPreviouslyFailingTest extends FunSuite {
  val folder = "src/test/resources/cdcl_solver/previously_failing"

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
    test("cdcl_solver_" + f) {
      val formula = CNFConversionTestUtils.readSmt2File(folder, f.split(".smt2")(0))
      assert(SolverValidator.solveFormulaAndValidate(formula, new CDCLSolver))
    }
  }

  /**
    * for debugging...
    */
  test("dummy_test") {
    // paste number of failing test here to debug manually
    val testNr = "277ea7b1-d67c-40a7-a09d-129bce3fb293"

    val formula = CNFConversionTestUtils.readSmt2File(folder, testNr)
    assert(SolverValidator.solveFormulaAndValidate(formula, new CDCLSolver))
  }
}
