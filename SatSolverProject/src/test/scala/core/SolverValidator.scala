package core

import smtlib.parser.Terms.Term
import util.PropositionalLogic

/**
  * Created by Severin on 2017-03-06.
  */
object SolverValidator {
  /**
    * Solves given formula in cnf using the given algorithm and validates the result.
    *
    * @param formula propositional logic formula in cnf
    * @param solver  the SatSolver to use
    * @return whether the dp algorithms provided the correct result or not
    */
  def solveFormulaAndValidate(formula: Term, solver: SATSolvingAlgorithm): Boolean = {

    // is the formula in cnf?
    if (!PropositionalLogic.isCNF(formula)) {
      println("Formula not in CNF!")
      return false
    }

    val dpResult: Option[Map[String, Boolean]] = solver.checkSAT(formula)
    // is the result the same as z3 solver gives / is the model valid?
    if (!SolverTestUtils.checkSATResult(dpResult, formula)) {
      println("The result from " + solver.getClass.getSimpleName + " is not the same the z3 solver gives.")
      return false
    }
    true
  }
}
