package core

import org.scalatest.FunSuite
import util.Smt2FileUtils

/**
  * Created by jan on 03.03.17.
  *
  * Tests if formulas are correctly converted into CNF.
  *
  * Creates new random formulas and tests them, formulas that don't pass the test get stored and will be tested later
  * again by the CNFConversionPreviouslyFailingTests test.
  *
  */
class CNFConversionRandomTest extends FunSuite {

  val folder = "src/test/resources/cnf_conversion/previously_failing/"
  val numberOfRandomFormulas = 250
  val storeFailedFormulas = true

  // random formula generator parameters
  val numberOfVariables = 100
  val maxChildCount = 25
  val minDepth = 4
  val maxDepth = 8

  val generator = new RandomFormulaGenerator(numberOfVariables, maxChildCount, minDepth, maxDepth)
  for (i <- 1 to numberOfRandomFormulas) {
    test("newRandomFormula_" + i) {
      val formula = generator.generateRandomFormula()
      val correct = CNFConversionValidator.convertToCnfAndValidate(formula)
      if (!correct && storeFailedFormulas) {
        // write formula to file
        Smt2FileUtils.writeFormulaToSmt2File(formula,
          folder + java.util.UUID.randomUUID.toString + ".smt2", getModel = true)
      }
      assert(correct)
    }
  }

}
