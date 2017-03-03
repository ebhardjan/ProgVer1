package core

import org.scalatest.FunSuite

/**
  * Created by jan on 03.03.17.
  *
  * Tests if formulas are correctly converted into CNF.
  */
class CNFConversionTest extends FunSuite {

  val folder = ""

  test("dummy_test") {
    val formula = CNFConversionTestUtils.readSmt2File(folder,
      "dummy_test")
    val cnf = CNFConversion.toCNF(formula)
    assert(CNFConversionTestUtils.formulasEqual(formula, cnf))
  }

}
