package core

import org.scalatest.FunSuite
import smtlib.parser.Terms.Term
import util.Smt2FileUtils

/**
  * Created by jan on 01.03.17.
  *
  * Tests if the cnf_conversion.stages of the cnf conversion are correct.
  * The tests in this file have a very high coverage in the CNFConversion class and test most of the special cases.
  */
class CNFConversionStagesTest extends FunSuite {

  val folder = "src/test/resources/cnf_conversion/stages"

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

  test("push_down_negations_simple6") {
    val cnf = formulaAfterNegationPushDown("push_down_negations_simple6")
    assertEqualsIgnoringWhitespaces(cnf.toString, "(and (not p3) (not p4) (not p3))")
  }

  test("push_down_negations_exception") {
    intercept[IllegalStateException] {
      formulaAfterNegationPushDown("implies_simple1")
    }
  }

  test("distribute_conjuncts_over_disjuncts_simple1") {
    val cnf = formulaAfterDistributeConjunctsOverDisjuncts("distribute_conjuncts_over_disjuncts_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(and (or a b c e) (or a b d e) (or a b c f) (or a b d f))")
  }

  test("distribute_conjuncts_over_disjuncts_simple2") {
    var cnf = formulaAfterDistributeConjunctsOverDisjuncts("distribute_conjuncts_over_disjuncts_simple2")
    cnf = CNFConversion.removeDuplicateClausesAndDuplicateLiterals(cnf)
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(and (or a b c e) (or a b d e) (or a b c f) (or a b d f))")
  }

  test("distribute_conjuncts_over_disjuncts_simple3") {
    val cnf = formulaAfterDistributeConjunctsOverDisjuncts("distribute_conjuncts_over_disjuncts_simple3")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(and (or a b c) (or a b d) (or a e))")
  }

  test("distribute_conjuncts_over_disjuncts_simple4") {
    val cnf = formulaAfterDistributeConjunctsOverDisjuncts("distribute_conjuncts_over_disjuncts_simple4")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(or a b c)")
  }

  test("distribute_conjuncts_over_disjuncts_exception") {
    intercept[IllegalStateException] {
      formulaAfterDistributeConjunctsOverDisjuncts("implies_simple1")
    }
  }

  test("top_elimination_simple1") {
    val cnf = formulaAfterTopAndBottomRemoval("top_elimination_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString, "true")
  }

  test("top_elimination_simple2") {
    val cnf = formulaAfterTopAndBottomRemoval("top_elimination_simple2")
    assertEqualsIgnoringWhitespaces(cnf.toString, "true")
  }

  test("top_elimination_simple3") {
    val cnf = formulaAfterTopAndBottomRemoval("top_elimination_simple3")
    assertEqualsIgnoringWhitespaces(cnf.toString, "p1")
  }

  test("bottom_elimination_simple1") {
    val cnf = formulaAfterTopAndBottomRemoval("bottom_elimination_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString, "false")
  }

  test("bottom_elimination_simple2") {
    val cnf = formulaAfterTopAndBottomRemoval("bottom_elimination_simple2")
    assertEqualsIgnoringWhitespaces(cnf.toString, "false")
  }

  test("bottom_elimination_simple3") {
    val cnf = formulaAfterTopAndBottomRemoval("bottom_elimination_simple3")
    assertEqualsIgnoringWhitespaces(cnf.toString, "p1")
  }

  test("bottom_and_top_elimination_simple1") {
    val cnf = formulaAfterTopAndBottomRemoval("bottom_and_top_elimination_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString, "true")
  }

  test("bottom_and_top_elimination_exception") {
    intercept[IllegalStateException] {
      formulaAfterTopAndBottomRemoval("implies_simple1")
    }
  }

  test("duplicate_clause_elimination_simple1") {
    val cnf = formulaAfterDuplicateElimination("duplicate_clause_elimination_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(or p1 p2)")
  }

  test("duplicate_literal_elimination_simple1") {
    val cnf = formulaAfterDuplicateElimination("duplicate_literal_elimination_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(and p1 (not p1))")
  }

  test("duplicate_removal_exception") {
    intercept[IllegalStateException] {
      formulaAfterDuplicateElimination("implies_simple1")
    }
  }

  test("and_flattening_simple1") {
    val cnf = formulaAfterFlattening("and_flattening_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(and p1 p2 p3 p4)")
  }

  test("and_flattening_simple2") {
    val cnf = formulaAfterFlattening("and_flattening_simple2")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(and p1 p2 p3 p4)")
  }

  test("and_flattening_simple3") {
    val cnf = formulaAfterFlattening("and_flattening_simple3")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(and a b (or c d))")
  }

  test("or_flattening_simple1") {
    val cnf = formulaAfterFlattening("or_flattening_simple1")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(or p1 p2 p3 p4)")
  }

  test("or_flattening_simple2") {
    val cnf = formulaAfterFlattening("or_flattening_simple2")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(or p1 p2 p3 p4)")
  }

  test("or_flattening_simple3") {
    val cnf = formulaAfterFlattening("or_flattening_simple3")
    assertEqualsIgnoringWhitespaces(cnf.toString,
      "(or a b (and c d))")
  }

  def assertEqualsIgnoringWhitespaces(actual: String, expected: String): Unit = {
    val actual_ = actual.replaceAll("\\s+", "")
    val expected_ = expected.replaceAll("\\s+", "")

    assert(actual_ == expected_)
  }

  /**
    * returns the formula after applying the "remove implications" step only
    */
  def formulaAfterImplicationRemoval(filename: String): Term = {
    val formula = Smt2FileUtils.readSmt2File(folder, filename)
    CNFConversion.replaceImplication(formula)
  }

  /**
    * returns the formula after applying the "push negations inwards" step only
    */
  def formulaAfterNegationPushDown(filename: String): Term = {
    val formula = Smt2FileUtils.readSmt2File(folder, filename)
    CNFConversion.pushDownNegations(formula)
  }

  /**
    * returns the formula after applying the "distribute disjuncts over conjuncts" step only
    */
  def formulaAfterDistributeConjunctsOverDisjuncts(filename: String): Term = {
    val formula = Smt2FileUtils.readSmt2File(folder, filename)
    CNFConversion.distributeConjunctionsOverDisjunctions(formula)
  }

  /**
    * returns the formula after applying the "remove top and bottom" step only
    */
  def formulaAfterTopAndBottomRemoval(filename: String): Term = {
    val formula = Smt2FileUtils.readSmt2File(folder, filename)
    CNFConversion.removeTopAndBottom(formula)
  }

  /**
    * returns the formula after duplicate clause and duplicate literal elimination
    *
    * @return
    */
  def formulaAfterDuplicateElimination(filename: String): Term = {
    val formula = Smt2FileUtils.readSmt2File(folder, filename)
    CNFConversion.removeDuplicateClausesAndDuplicateLiterals(formula)
  }

  /**
    * returns the formula after flattening
    *
    * @return
    */
  def formulaAfterFlattening(filename: String): Term = {
    val formula = Smt2FileUtils.readSmt2File(folder, filename)
    CNFConversion.flatten(formula)
  }

}
