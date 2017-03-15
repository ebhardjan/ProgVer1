package core

import smtlib.parser.Terms.Term
import util.{CNFRepresentation, InternalCNF, InternalLiteral, SolverUtils}

import scala.collection.mutable

/**
  * Created by Severin on 2017-03-09.
  *
  * Solve the SAT problem given a formula using the DPLL algorithm.
  */
class DPLLSolver extends SATSolvingAlgorithm {

  /**
    * Applies the DPLL algorithm to the given formula. Returns None in case of unsat, otherwise the model mapping
    * variables to booleans.
    */
  override def checkSAT(formula: Term): Option[Map[String,Boolean]] = {
    val cnfRep: InternalCNF = CNFRepresentation.convertCNFToInternal(formula)
    val model: Map[String,Boolean] = Map()
    checkSAT(cnfRep, model) match {
      case None => None
      case Some(map) => Some(SolverUtils.setAllRemainingVariables(cnfRep, map))
    }
  }

  /**
    * Recursively apply the DPLL steps until the SAT question is answered.
    */
  def checkSAT(formula: InternalCNF, model: Map[String, Boolean]): Option[Map[String, Boolean]] = {
    if (SolverUtils.containsEmptyClause(formula)) {
      return None
    } else if (formula.conjuncts.isEmpty) {
      return Some(model)
    }
    removeTautologies(formula, model) match {
      case Some(f) => return checkSAT(f, model)
      case None =>
    }
    applyUnitPropagation(formula, model) match {
      case Some((f, r)) =>
        return checkSAT(f, model + r)
      case None =>
    }
//    applyPureLiteralRule(formula, model) match {
//      case Some((f, r)) =>
//        return checkSAT(f, model + r)
//      case None =>
//    }
    applyDecisionRule(formula, model)
  }

  /**
    * Apply the decision rule. Pick a victim literal and try to solve the problem given that the victim was set to true.
    * If we get sat, return the model. If we get unsat, set the victim literal to false and solve the new sub-problem.
    */
  def applyDecisionRule(formula: InternalCNF, model: Map[String, Boolean]): Option[Map[String, Boolean]] = {
    val victim: String = pickVictim(formula)
    val chooseTrueFormula = InternalCNF(
      SolverUtils.removeLiteralFromClauses(
        SolverUtils.takeClausesNotContainingLiteral(formula.conjuncts, InternalLiteral(true, victim)),
        InternalLiteral(false, victim)
      )
    )
    checkSAT(chooseTrueFormula, model + (victim -> true)) match {
      case Some(m) => Some(m)
      case None =>
        val chooseFalseFormula = InternalCNF(
          SolverUtils.removeLiteralFromClauses(
            SolverUtils.takeClausesNotContainingLiteral(formula.conjuncts, InternalLiteral(false, victim)),
            InternalLiteral(true, victim)
          )
        )
        checkSAT(chooseFalseFormula, model + (victim -> false))
    }
  }

  /**
    * Pick a victim literal on which to do the decision.
    */
  def pickVictim(formula: InternalCNF): String = {
    formula.conjuncts.head.disjuncts.head.literal.name
//    pickMostOccurringVictim(formula)
  }

  /**
    * Find the literal which occurs most often in the formula.
    */
  def pickMostOccurringVictim(formula: InternalCNF): String = {
    val varOccurrenceMap: mutable.Map[String, Int] = mutable.Map()
    for (c <- formula.conjuncts) {
      for (d <- c.disjuncts; l = d.literal) {
        varOccurrenceMap.getOrElse(l.name, None) match {
          case None => varOccurrenceMap.put(l.name, 1)
          case occurrences: Int => varOccurrenceMap(l.name) = occurrences + 1
        }
      }
    }
    varOccurrenceMap.toSeq.sortBy(-_._2).head._1
  }
}
