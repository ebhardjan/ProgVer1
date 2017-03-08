package core

import smtlib.parser.Terms.Term
import util.{CNFRepresentation, SolverUtils}

import sys.process._

/**
  * Created by Severin on 2017-03-07.
  */
object SolverTestUtils {
  /**
    * Check a candidate model for correctness, if there is no model, run z3 solver to check if the formula is unsat.
    *
    * @param result The result to be checked for correctness. None for unsat, model for sat.
    * @param formula The formula to be checked. Must be in cnf.
    * @return True if the z3 solver returns the same result as 'result', false otherwise.
    */
  def checkSATResult(result: Option[Map[String, Boolean]], formula: Term): Boolean = {
    result match {
      case None => val filename1 = CNFConversionTestUtils.writeFormulaToSmt2File(formula, "tmp.smt2", getModel = false)
          val z3result: String = "z3 -smt2 " + filename1 !!;
          z3result.contains("unsat")
      case Some(map) => checkModelForCorrectness(formula, map)
    }
  }

  /**
    * Check if the given model makes the formula true.
    *
    * @param formula The formula to be checked.
    * @param model The model for the formula.
    * @return True iff the model makes the formula true.
    */
  def checkModelForCorrectness(formula: Term, model: Map[String, Boolean]): Boolean = {
    val internal = CNFRepresentation.convertCNFToInternal(formula)
    internal.conjuncts.forall(clause => SolverUtils.evaluateClause(clause, model))
  }

}
