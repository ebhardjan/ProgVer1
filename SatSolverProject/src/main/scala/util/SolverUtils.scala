package util

import scala.collection.immutable.Set

/**
  * Created by Severin on 2017-03-07.
  */
object SolverUtils {
  /**
    * Evaluate the given clause under the model.
    *
    * @param clause The clause to be evaluated.
    * @param model The model for the clause.
    * @return The truth value of the clause under the given model.
    */
  def evaluateClause(clause: InternalClause, model: Map[String, Boolean]): Boolean = {
    clause.disjuncts.foldLeft[Boolean](false)((b, disjunct: InternalDisjunct) => b || {
      model getOrElse(disjunct.literal.name, None) match {
        case None => false
        case value => disjunct.literal.polarity == value
      }
    }) || isTautology(clause)
  }

  /**
    * Check if a clause is a tautology (conatains literal in both polarities)
    *
    * @param clause The clause to check.
    * @return True if the clause is a tautology, false otherwise.
    */
  def isTautology(clause: InternalClause): Boolean = {
    (for (d @ InternalDisjunct(InternalLiteral(polarity, name), isActive) <- clause.disjuncts if {
      isActive && (clause.disjuncts contains InternalDisjunct(InternalLiteral(!polarity, name), true))
    }) yield d).isEmpty
  }

  /**
    * Check if a formula contains an empty clause, meaning one having no active elements.
    * Note: a CNF formula containing an empty clause is unsatisfiable.
    *
    * @param formula The formula to check.
    * @return true if there is an empty clause, false otherwise.
    */
  def containsEmptyClause(formula: InternalCNF) : Boolean = {
    formula.conjuncts.foldLeft[Boolean](false)((l, clause) => l || clause.disjuncts.forall(d => !d.isActive))
  }

  /**
    * From a set of clauses, take only those which contain a certain literal.
    *
    * @param set The initial set of clauses.
    * @param literal The literal which has to be in all remaining clauses.
    * @return The new set of clauses.
    */
  def takeClausesContainingLiteral(set: Set[InternalClause], literal: InternalLiteral) :
  Set[InternalClause] = {
    for (clause <- set if {
      (for (disjunct @ InternalDisjunct(InternalLiteral(pol, varName), isActive) <- clause.disjuncts if {
        varName == literal.name && pol == literal.polarity && isActive
      }) yield disjunct).nonEmpty
    }) yield clause
  }

  /**
    * From a set of clauses, take only those which do not contain a certain literal.
    *
    * @param set The initial set of clauses.
    * @param literal The literal which can't occurr in any of the remaining clauses.
    * @return The new set of clauses.
    */
  def takeClausesNotContainingLiteral(set: Set[InternalClause], literal: InternalLiteral) :
  Set[InternalClause] = {
    for (clause <- set if {
      (for (disjunct @ InternalDisjunct(InternalLiteral(pol, varName), isActive) <- clause.disjuncts if {
        varName == literal.name && pol == literal.polarity && isActive
      }) yield disjunct).isEmpty
    }) yield clause
  }

  /**
    * From a set of clauses, return a set containing the same clauses with a given literal removed from all of them.
    *
    * @param clauses The initial set of clauses.
    * @param literal The literal to be removed from all clauses.
    * @return The resulting set of clauses.
    */
  def removeLiteralFromClauses(clauses: Set[InternalClause], literal: InternalLiteral): Set[InternalClause] = {
    for (c <- clauses) yield InternalClause(for (d <- c.disjuncts if d.literal != literal) yield d)
  }

  /**
    * Find a pure literal in the formula. A literal is pure if it occurs only positively or only negatively in the
    * formula. Return None if no such literal exists, otherwise return the first one found together with its polarity.
    *
    * @param formula The formula to search for a pure literal.
    * @return None if no pure literal exists, otherwise the polarity and name of a pure literal.
    */
  def findPureLiteral(formula: InternalCNF): Option[(Boolean,String)] = {
    val positives: Set[String] = formula.vars(true)
    val negatives: Set[String] = formula.vars(false)
    if ((positives -- negatives).nonEmpty) {
      Some(true, (positives -- negatives).head)
    } else if ((negatives -- positives).nonEmpty) {
      Some(false, (negatives -- positives).head)
    } else {
      None
    }
  }
}
