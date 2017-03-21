package core

import java.io.File

import org.scalatest.FunSuite

/**
  * Created by Severin on 2017-03-07.
  *
  * Tests if previously failing formulas are now correctly evaluated by the DP algorithm.
  *
  * The dummy_test test case is useful for debugging.
  */
class DPSolverPreviouslyFailingTest extends FunSuite {
  val folder = "src/test/resources/dp_solver/previously_failing"

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
    * here we create as many tests as there are files in the specified folders
    */
  for (f <- getListOfSmt2Files(folder)) {
    test("previouslyFailing_" + f) {
      val formula = CNFConversionTestUtils.readSmt2File(folder, f.split(".smt2")(0))
      assert(SolverValidator.solveFormulaAndValidate(formula, new DPSolver))
    }
  }

  /**
    * for debugging...
    */
  ignore("dummy_test") {
    // paste uuid of failing test here to debug manually
    val uuid = "306ac934-58f6-42c8-935d-4ea6321f1fe0"

    val formula = CNFConversionTestUtils.readSmt2File(folder, uuid)
    assert(SolverValidator.solveFormulaAndValidate(formula, new DPSolver))
  }
}
