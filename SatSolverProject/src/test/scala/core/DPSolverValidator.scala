package core

import smtlib.parser.Terms.Term
import util.PropositionalLogic

/**
  * Created by Severin on 2017-03-06.
  */
object DPSolverValidator {
  /**
    * Solves given formula in cnf using DP Algorithm and validates the result.
    *
    * @param formula propositional logic formula in cnf
    * @return whether the dp algorithms provided the correct result or not
    */
  def solveFormulaAndValidate(formula: Term): Boolean = {

    // is the formula now really in cnf?
    if (!PropositionalLogic.isCNF(formula)) {
      println("Formula not in CNF!")
      return false
    }

    val dpResult: Option[Map[String, Boolean]] = DPSolver.checkSAT(formula)
    // is the formula equivalent to the original one?
    if (!SolverTestUtils.checkSATResult(dpResult, formula)) {
      println("The result from DP is not the same the z3 solver gives.")
      return false
    }
    true
  }
}
