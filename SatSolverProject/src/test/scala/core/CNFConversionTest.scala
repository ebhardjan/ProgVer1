package core

import java.io.File

import org.scalatest.FunSuite
import smtlib.parser.Terms.Term
import smtlib.theories.Core.{False, True}
import util.PropositionalLogic

/**
  * Created by jan on 03.03.17.
  *
  * Tests if formulas are correctly converted into CNF.
  *
  * Creates new random formulas and tests them, formulas that don't pass the test get stored and will be tested later
  * again by the previouslyFailingFormulas test.
  *
  * The dummy_test test case is useful for debugging.
  */
class CNFConversionTest extends FunSuite {

  val folder = "src/test/resources/cnf_conversion/previously_failing/"
  val numberOfRandomFormulas = 250
  val storeFailedFormulas = true

  // random formula generator
  val numberOfVariables = 15
  val maxChildCount = 5
  val minDepth = 6
  val maxDepth = 8

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

  private[this] def cnfConversionCorrect(formula: Term): Boolean = {
    var cnf = False()

    // convert to cnf and return false in case an error occurred
    try {
      cnf = CNFConversion.toCNF(formula)
    } catch {
      case _: Throwable => return false
    }

    // is the formula now really in cnf?
    if (!PropositionalLogic.isCNF(cnf)) {
      print("Formula not in CNF! -> ")
      return false
    }

    // is the formula equivalent to the original one?
    if (!CNFConversionTestUtils.formulasEqual(formula, cnf)) {
      print("Formula in CNF but transformed formula is not equivalent to the original one! -> ")
      return false
    }
    true
  }

  test("previouslyFailingFormulas") {
    val fileNames = getListOfSmt2Files(folder)
    var passedCount = 0
    for (f <- fileNames) {
      val formula = CNFConversionTestUtils.readSmt2File(folder, f.split(".smt2")(0))
      val correct = cnfConversionCorrect(formula)
      if (!correct) {
        println("still incorrect: " + f)
      } else {
        passedCount += 1
        println(passedCount + " / " + fileNames.length + " correct")
      }
    }
    assert(passedCount == fileNames.length,
      "-> " + (fileNames.length - passedCount) + " formulas still do not pass!")
  }

  test("testRandomNewFormulas") {
    var passedCount = 0
    val generator = new RandomFormulaGenerator(numberOfVariables, maxChildCount, minDepth, maxDepth)
    for (i <- 1 to numberOfRandomFormulas) {
      //TODO introduce timeout and treat formulas that timed out as "failed"
      val formula = generator.generateRandomFormula()
      val correct = cnfConversionCorrect(formula)
      if (!correct) {
        // write formula to file
        if (storeFailedFormulas) {
          CNFConversionTestUtils.writeFormulaToSmt2File(formula,
            folder + java.util.UUID.randomUUID.toString + ".smt2", getModel = true)
        }
        println("incorrect")
      } else {
        passedCount += 1
        println(passedCount + " / " + numberOfRandomFormulas + " correct")
      }
    }
    assert(passedCount == numberOfRandomFormulas,
      "-> " + (numberOfRandomFormulas - passedCount) + " random formulas did not pass!")
  }

  test("dummy_test") {
    // paste uuid of failing test here to debug manually
    val uuid = "04effc19-193a-47a9-89f1-64167bd8f36f"

    val formula = CNFConversionTestUtils.readSmt2File(folder, uuid)
    assert(cnfConversionCorrect(formula))
  }

}
