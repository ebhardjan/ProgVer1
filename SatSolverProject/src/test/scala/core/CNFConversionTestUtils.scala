package core

import smtlib.parser.Terms.Term
import smtlib.theories.Core._
import util.Smt2FileUtils

import scala.sys.process._

/**
  * Created by jan on 03.03.17.
  *
  * Util methods for the CNFConversion tests
  */
object CNFConversionTestUtils {

  /**
    * checks if two formulas are equal by running z3 solver
    */
  def formulasEqual(formula1: Term, formula2: Term): Boolean = {
    val one = And(formula1, Not(formula2))
    val filename1 = Smt2FileUtils.writeFormulaToSmt2File(one, "tmp.smt2", getModel = false)
    val res1 = "z3 -smt2 " + filename1 !!

    val two = And(Not(formula1), formula2)
    val filename2 = Smt2FileUtils.writeFormulaToSmt2File(two, "tmp.smt2", getModel = false)
    val res2 = "z3 -smt2 " + filename2 !!

    res1.contains("unsat") || res2.contains("unsat")
  }

}
