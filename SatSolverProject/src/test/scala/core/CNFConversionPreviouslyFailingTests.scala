package core

import org.scalatest.FunSuite

/**
  * Created by jan on 06.03.17.
  *
  * Tests if previously failing formulas are now correctly converted into CNF.
  *
  * The dummy_test test case is useful for debugging.
  */
class CNFConversionPreviouslyFailingTests extends FunSuite {

  val folder = "src/test/resources/cnf_conversion/previously_failing/"

  /**
    * here we create as many tests as there are files in the specified folders
    */
  for (f <- TestUtils.getListOfSmt2Files(folder)) {
    test("previouslyFailing_" + f) {
      val formula = CNFConversionTestUtils.readSmt2File(folder, f.split(".smt2")(0))
      assert(CNFConversionValidator.convertToCnfAndValidate(formula))
    }
  }

  /**
    * for debugging...
    */
  ignore("dummy_test") {
    // paste uuid of failing test here to debug manually
    val uuid = "04effc19-193a-47a9-89f1-64167bd8f36f"

    val formula = CNFConversionTestUtils.readSmt2File(folder, uuid)
    assert(CNFConversionValidator.convertToCnfAndValidate(formula))
  }

}
