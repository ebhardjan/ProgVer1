package core

import smtlib.parser.Terms.Term
import util._

import scala.collection.immutable.Set

/**
  * Created by Severin on 2017-03-01.
  *
  * Solve the SAT problem given a formula using the Davis-Putnam algorithm.
  */
object DPSolver extends SATSolvingAlgorithm {

  /**
    * Store clauses removed from formula by resolution rule together with the variable name which was the target of the
    * resolution. The set contains the clauses where the variable occurred positively.
    */
  var resolutionClauseStack: List[(String, Set[InternalClause])] = List()

  /**
    * Applies the DP algorithm to the given formula. Returns None in case of unsat, otherwise the model mapping
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
    * Recursively apply the DP steps until the SAT question is answered.
    */
  def checkSAT(formula: InternalCNF, model: Map[String, Boolean]): Option[Map[String, Boolean]] = {
    if (SolverUtils.containsEmptyClause(formula)) {
      return None
    } else if (formula.conjuncts.isEmpty) {
      // There are no more conjuncts -> the formula is satisfiable. If there are still unresolved variables on the
      // stack, resolve them.
      if (resolutionClauseStack.nonEmpty) {
        return Some(resolveMissingAssignments(model))
      } else {
        return Some(model)
      }
    } else if (formula.conjuncts.size == 1) {
      // If there is only one conjunct left, the formula is satisfiable and we add all literals in that last conjunct
      // to the model.
      return checkSAT(InternalCNF(Set()), addLiteralsToModel(formula.conjuncts.head, model))
    }
    removeTautologies(formula, model) match {
      case Some((f, m)) => return checkSAT(f, m)
      case None =>
    }
    applyUnitPropagation(formula, model) match {
      case Some((f, m)) => return checkSAT(f, m)
      case None =>
    }
    applyPureLiteralRule(formula, model) match {
      case Some((f, m)) => return checkSAT(f, m)
      case None =>
    }
    applyResolutionRule(formula, model) match {
      case Some((f, m)) => return checkSAT(f, m)
    }
  }

  /**
    * Apply the resolution rule. Pick a victim literal from the formula and do resolution on it. Add the victim variable
    * together with all the clauses where it occurred positively on the resolutionClauseStack for later resolving its
    * truth value.
    */
  def applyResolutionRule(formula: InternalCNF, model: Map[String, Boolean])
  : Option[(InternalCNF, Map[String, Boolean])] = {
    // Pick a literal to do resolution on
    val victimLiteral = pickVictimLiteral(formula)
    // Find all clauses where the victim literal appears positively and negatively respectively
    val positiveClauses = SolverUtils.takeClausesContainingLiteral(formula.conjuncts,
      InternalLiteral(true, victimLiteral.name))
    val negativeClauses = SolverUtils.takeClausesContainingLiteral(formula.conjuncts,
      InternalLiteral(false, victimLiteral.name))
    // Add a removed clause to stack for later resolving the variable assignment.
    resolutionClauseStack = (victimLiteral.name, positiveClauses) :: resolutionClauseStack
    // Add new clauses to formula
    val newClauses = for {pc <- positiveClauses
                          nc <- negativeClauses} yield buildNewResolutionClause(pc, nc, victimLiteral.name)
    val newFormula: InternalCNF = InternalCNF(formula.conjuncts -- positiveClauses -- negativeClauses ++ newClauses)
    Some((newFormula, model))
  }

  /**
    * Return a literal on which to do resolution. For now just return first occurring literal.
    */
  def pickVictimLiteral(formula: InternalCNF): InternalLiteral = {
    formula.conjuncts.head.disjuncts.head.literal
  }

  /**
    * Take two clauses where literal with name 'name' occurs positively in one and negatively in the other.
    * Return a clause containing all other literals in first and second.
    */
  def buildNewResolutionClause(first: InternalClause, second: InternalClause, name: String): InternalClause = {
    val firstDisjuncts = for (d <- first.disjuncts if d.literal.name != name) yield d
    val secondDisjuncts = for (d <- second.disjuncts if d.literal.name != name) yield d
    InternalClause(firstDisjuncts ++ secondDisjuncts)
  }

  /**
    * Go through all the clauses in 'resolutionClauseStack' and add the correct value to the model.
    */
  def resolveMissingAssignments(model: Map[String,Boolean]): Map[String,Boolean] = {
    resolutionClauseStack match {
      case List() => model
      case (varName, positiveClauses) :: remainingList =>
        resolutionClauseStack = remainingList
        val strippedPositives = SolverUtils.removeLiteralFromClauses(positiveClauses, InternalLiteral(true, varName))
        val hasFalsePositiveClause = strippedPositives.foldLeft[Boolean](false)((b, clause) => {
          b || !SolverUtils.evaluateClause(clause, model)
        })
        resolveMissingAssignments(model + (varName -> hasFalsePositiveClause))
    }
  }

}
