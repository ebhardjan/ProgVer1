package core

import org.scalatest.FunSuite
import smtlib.parser.Commands.Command
import smtlib.parser.Terms.Term

/**
  * Created by jan on 01.03.17.
  */
class CNFConversionTest extends FunSuite {

  test("implies_simple1") {
    val cnf = formulaAfterImplicationRemoval("implies_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString, "(or (not p1) (not p2))")
  }

  test("implies_simple2") {
    val cnf = formulaAfterImplicationRemoval("implies_simple2")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(and (or (not (not p1)) p2) (or (not p1) (not p2)))")
  }

  test("implies_simple3") {
    val cnf = formulaAfterImplicationRemoval("implies_simple3")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(or (not (and p1 p2)) p3)")
  }

  test("implies_simple4") {
    val cnf = formulaAfterImplicationRemoval("implies_simple4")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(or (not (or (not p1) p2)) p2)")
  }

  test("push_down_negations_simple1") {
    val cnf = formulaAfterNegationPushDown("push_down_negations_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString, "p1")
  }

  test("push_down_negations_simple2") {
    val cnf = formulaAfterNegationPushDown("push_down_negations_simple2")
    assertEqualsIgnoringWhitespaces(cnf.toString, "(and p1 p2)")
  }

  test("push_down_negations_simple3") {
    val cnf = formulaAfterNegationPushDown("push_down_negations_simple3")
    assertEqualsIgnoringWhitespaces(cnf.toString, "(or p1 p2)")
  }

  test("push_down_negations_simple4") {
    val cnf = formulaAfterNegationPushDown("push_down_negations_simple4")
    assertEqualsIgnoringWhitespaces(cnf.toString, "(or (and (not p1) (not p2)) (not p3))")
  }

  test("push_down_negations_simple5") {
    val cnf = formulaAfterNegationPushDown("push_down_negations_simple5")
    assertEqualsIgnoringWhitespaces(cnf.toString, "(and (and (not p1) (not p2)) (not p3))")
  }

  test("distribute_conjuncts_over_disjuncts_simple1") {
    val cnf = formulaAfterDistributeConjunctsOverDisjuncts("distribute_conjuncts_over_disjuncts_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(and (or a b c e) (or a b d e) (or a b c f) (or a b d f))")
  }

  def assertEqualsIgnoringWhitespaces(actual: String, expected: String): Unit = {
    val actual_ = actual.replaceAll("\\s+", "")
    val expected_ = expected.replaceAll("\\s+", "")

    assert(actual_ == expected_)
  }

  /**
    * reads an Smt2 file from the given filename and returns the formula
    */
  def readSmt2File(filename: String): Term = {
    val path = "src/test/resources/cnf_conversion/" + filename + ".smt2"
    val inputString = {
      val source = scala.io.Source.fromFile(path)
      try source.mkString finally source.close()
    }

    val script: List[Command] = MySATSolver.parseInputString(inputString)
    val (_, formula) = util.InputProcessing.processCommands(script)

    formula
  }

  /**
    * returns the cnf formula from a given file name
    */
  def getCnfFromFile(filename: String): Term = {
    val nonCnfFormula = readSmt2File(filename)
    CNFConversion.toCNF(nonCnfFormula)
  }

  /**
    * returns the formula after applying the "remove implications" step only
    */
  def formulaAfterImplicationRemoval(filename: String): Term = {
    val formula = readSmt2File(filename)
    CNFConversion.replaceImplication(formula)
  }

  /**
    * returns the formula after applying the "push negations inwards" step only
    */
  def formulaAfterNegationPushDown(filename: String): Term = {
    val formula = readSmt2File(filename)
    CNFConversion.pushDownNegations(formula)
  }

  /**
    * returns the formula after applying the "distribute disjuncts over conjuncts" step only
    */
  def formulaAfterDistributeConjunctsOverDisjuncts(filename: String): Term = {
    val formula = readSmt2File(filename)
    CNFConversion.distributeConjunctionsOverDisjunctions(formula)
  }

}
