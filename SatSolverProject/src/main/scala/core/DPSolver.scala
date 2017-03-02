package core

import smtlib.parser.Terms.Term
import smtlib.theories.Core.True
import util._

import scala.collection.immutable.Set

/**
  * Created by Severin on 2017-03-01.
  *
  * Solve the SAT problem given a formula using the Davis-Putnam algorithm.
  */
object DPSolver extends SATSolvingAlgorithm {
  // Returns None in case of unsat, otherwise the model mapping variables to booleans.
  override def checkSAT(formula : Term): Option[Map[String,Boolean]] = {
    if (formula == True()) {
      // First rule: if the formula is Top already, return sat
      Some(Map())
    } else {
      var cnfRep : InternalCNF = CNFRepresentation.convertCNFToInternal(formula)
      var model : Map[String,Boolean] = Map()
      // Iterate until there is an empty clause (unsat) or there are no more clauses (sat)
      while (!cnfRep.containsEmptyClause() && cnfRep.conjuncts.nonEmpty) {
        val result = applyPureLiteralRule(cnfRep, model)
        cnfRep = result._1
        model = result._2
        // TODO: implement other rules
      }
      if (cnfRep.containsEmptyClause()) {
        None
      } else {
        Some(model)
      }
    }
  }

  // Apply the pure literal rule. If there is a pure literal, set its value in the model and remove all clauses
  // which contain the literal from the formula.
  def applyPureLiteralRule(formula : InternalCNF, model : Map[String,Boolean]) : (InternalCNF, Map[String,Boolean]) = {
    val pureLiteral = findPureLiteral(formula)
    pureLiteral match {
      case None => (formula, model)
      case Some((literal, value)) =>
        // if there is a pure literal, remove all clauses containing it from the formula
        val newDisjuncts : Set[InternalClause] = for (c <- formula.conjuncts if {
          (for (d @ InternalDisjunct(InternalLiteral(_, varName), _) <- c.disjuncts if varName == literal)
            yield d).isEmpty
        }) yield c
        val newFormula : InternalCNF= InternalCNF(newDisjuncts)
        val newMap : Map[String,Boolean] = model + (literal -> value)
        (newFormula, newMap)
    }
  }

  // A literal is pure if it occurs only positively or only negatively in the formula. Return None if no such literal
  // exists, otherwise return the first one found together with its polarity.
  def findPureLiteral(formula: InternalCNF) : Option[(String,Boolean)] = {
    val positives : Set[String] = formula.vars(true)
    val negatives : Set[String] = formula.vars(false)
    if ((positives -- negatives).nonEmpty) {
      Some((positives -- negatives).head, true)
    } else if ((negatives -- positives).nonEmpty) {
      Some((negatives -- positives).head, false)
    } else {
      None
    }
  }

}
