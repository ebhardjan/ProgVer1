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

  // Store clauses removed from formula by resolution rule together with the variable name which was the target of the
  // resolution. The set contains the clauses where the variable occurred positively.
  var resolutionClauseStack: List[(String, Set[InternalClause])] = List()

  // Returns None in case of unsat, otherwise the model mapping variables to booleans.
  override def checkSAT(formula: Term): Option[Map[String,Boolean]] = {
    if (formula == True()) {
      // First rule: if the formula is Top already, return sat
      Some(Map())
    } else {
      var cnfRep: InternalCNF = CNFRepresentation.convertCNFToInternal(formula)
      var model: Map[String,Boolean] = Map()
      // Iterate until there is an empty clause (unsat) or there are no more clauses (sat)
      while (true) {
        var changed = false
        // println("Looping: \n" + cnfRep.toString)
        if (cnfRep.containsEmptyClause()) {
          return None
        } else if (cnfRep.conjuncts.isEmpty) {
          if (resolutionClauseStack.nonEmpty) model = resolveMissingAssignments(model)
          return Some(model)
        }
        var result = applyPureLiteralRule(cnfRep, model)
        cnfRep = result._1
        model = result._2
        changed = result._3
        if (!changed) {
          result = applyUnitPropagation(cnfRep, model)
          cnfRep = result._1
          model = result._2
          changed = result._3
        }
        if (!changed) {
          result = applyResolutionRule(cnfRep, model)
          cnfRep = result._1
          model = result._2
          changed = result._3
        }
      }
    }
    None
  }

  // Remove Tautologies from formula, i.e. all clauses where a literal appears positively and negatively.
  def simplifyTautologies(formula: InternalCNF, model: Map[String,Boolean]): (InternalCNF, Map[String,Boolean]) = {
    val newConjuncts = for (c <- formula.conjuncts if {
      (for (d @ InternalDisjunct(InternalLiteral(polarity, name), isActive) <- c.disjuncts if {
        isActive && (c.disjuncts contains InternalDisjunct(InternalLiteral(!polarity, name), true))
      }) yield d).isEmpty
    }) yield c
    if (newConjuncts.isEmpty) {
      // If a solution was found by doing the simplification, add true for the variable in the model.
      val removedVariables = for {c <- formula.conjuncts
                                  d @ InternalDisjunct(InternalLiteral(polarity, name), isActive) <- c.disjuncts if {
        isActive && (c.disjuncts contains InternalDisjunct(InternalLiteral(!polarity, name), true))
      }} yield d.literal
      val newAssignments = for (l @ InternalLiteral(polarity, _) <- removedVariables if polarity) yield l.name -> true
      val newModel = model ++ newAssignments
      (InternalCNF(Set()), newModel)
    } else {
      (InternalCNF(newConjuncts), model)
    }
  }

  // Apply the pure literal rule. If there is a pure literal, set its value in the model and remove all clauses
  // which contain the literal from the formula.
  def applyPureLiteralRule(formula: InternalCNF, model: Map[String,Boolean]) :
  (InternalCNF, Map[String,Boolean], Boolean) = {
    val pureLiteral = findPureLiteral(formula)
    pureLiteral match {
      case None => (formula, model, false)
      case Some((literal, value)) =>
        // if there is a pure literal, remove all clauses containing it from the formula
        val newConjuncts: Set[InternalClause] = takeClausesNotContainingLiteral(formula, literal, value)
        val newFormula: InternalCNF = InternalCNF(newConjuncts)
        val newModel: Map[String,Boolean] = model + (literal -> value)
        val simplified = simplifyTautologies(newFormula, newModel)
        (simplified._1, simplified._2, true)
    }
  }

  // Apply the unit propagation rule. If there is a clause with a single (active) literal, set this literal in the
  // model, remove every clause where it appears with the same polarity and remove (deactivate) the literal wherever
  // it appears with the opposite polarity.
  def applyUnitPropagation(formula: InternalCNF, model: Map[String,Boolean]) :
  (InternalCNF, Map[String,Boolean], Boolean) = {
    val unitClauses: Set[InternalClause] = for (c <- formula.conjuncts if {
      (for (d @ InternalDisjunct(_, true) <- c.disjuncts) yield d).size == 1
    }) yield c
    if (unitClauses.isEmpty) return (formula, model, false)
    val unitClause = unitClauses.head
    val variable: InternalDisjunct = (for (d <- unitClause.disjuncts if d.isActive) yield d).head
    // add the unit clause to the model
    val newModel = model + (variable.literal.name -> variable.literal.polarity)
    // remove all clauses which have the variable in the same polarity
    val newConjuncts: Set[InternalClause] =
      takeClausesNotContainingLiteral(formula, variable.literal.name, variable.literal.polarity)
    // remove (deactivate) the literal with opposite polarity from clauses
    for (c <- newConjuncts) {
      for (d <- c.disjuncts) {
        if (d.literal.name == variable.literal.name) {
          d.isActive = false
        }
      }
    }
    val simplified = simplifyTautologies(InternalCNF(newConjuncts), newModel)
    (simplified._1, simplified._2, true)
  }

  def applyResolutionRule(formula: InternalCNF, model: Map[String, Boolean]) :
  (InternalCNF, Map[String, Boolean], Boolean) = {
    // Pick a literal to do resolution on
    val victimLiteral = pickVictimLiteral(formula)
    // Find all clauses where the victim literal appears positively and negatively respectively
    val positiveClauses = takeClausesContainingLiteral(formula, victimLiteral.name, true)
    val negativeClauses = takeClausesContainingLiteral(formula, victimLiteral.name, false)
    // Add a removed clause to stack for later resolving the variable assignment.
    resolutionClauseStack = (victimLiteral.name, positiveClauses) :: resolutionClauseStack
    // Add new clauses to formula
    val newClauses = for {pc <- positiveClauses
                          nc <- negativeClauses} yield buildNewResolutionClause(pc, nc, victimLiteral.name)
    val newFormula: InternalCNF = InternalCNF(formula.conjuncts -- positiveClauses -- negativeClauses ++ newClauses)
    val simplified = simplifyTautologies(newFormula, model)
    (simplified._1, simplified._2, false)
  }

  // Return the set of clauses in formula which contain the literal with given name and polarity and is active.
  def takeClausesContainingLiteral(formula: InternalCNF, literalName: String, polarity: Boolean) :
  Set[InternalClause] = {
    for (clause <- formula.conjuncts if {
      (for (disjunct @ InternalDisjunct(InternalLiteral(pol, varName), isActive) <- clause.disjuncts if {
        varName == literalName && pol == polarity && isActive
      }) yield disjunct).nonEmpty
    }) yield clause
  }

  // Return the set of clauses in formula which do not contain the literal with given name and polarity and is active.
  def takeClausesNotContainingLiteral(formula: InternalCNF, literalName: String, polarity: Boolean) :
  Set[InternalClause] = {
    for (clause <- formula.conjuncts if {
      (for (disjunct @ InternalDisjunct(InternalLiteral(pol, varName), isActive) <- clause.disjuncts if {
        varName == literalName && pol == polarity && isActive
      }) yield disjunct).isEmpty
    }) yield clause
  }

  def removeLiteralFromClauses(clauses: Set[InternalClause], literal: InternalLiteral): Set[InternalClause] = {
    for (c <- clauses) yield InternalClause(for (d <- c.disjuncts if d.literal != literal) yield d)
  }

  def evaluateClause(clause: InternalClause, model: Map[String,Boolean]): Boolean = {
    clause.disjuncts.foldLeft[Boolean](false)((b, disjunct) => b || {
      if (disjunct.literal.polarity) {
        model(disjunct.literal.name)
      } else {
        !model(disjunct.literal.name)
      }})
  }

  // Return a literal on which to do resolution. For now just return first occurring literal.
  def pickVictimLiteral(formula: InternalCNF): InternalLiteral = {
    formula.conjuncts.head.disjuncts.head.literal
  }

  // Take two clauses where literal with name 'name' occurs positively in one and negatively in the other.
  // Return a clause containing all other literals in first and second.
  def buildNewResolutionClause(first: InternalClause, second: InternalClause, name: String): InternalClause = {
    val firstDisjuncts = for (d <- first.disjuncts if d.literal.name != name) yield d
    val secondDisjuncts = for (d <- second.disjuncts if d.literal.name != name) yield d
    InternalClause(firstDisjuncts ++ secondDisjuncts)
  }

  // A literal is pure if it occurs only positively or only negatively in the formula. Return None if no such literal
  // exists, otherwise return the first one found together with its polarity.
  def findPureLiteral(formula: InternalCNF): Option[(String,Boolean)] = {
    val positives: Set[String] = formula.vars(true)
    val negatives: Set[String] = formula.vars(false)
    if ((positives -- negatives).nonEmpty) {
      Some((positives -- negatives).head, true)
    } else if ((negatives -- positives).nonEmpty) {
      Some((negatives -- positives).head, false)
    } else {
      None
    }
  }

  // Go through all the clauses in 'resolutionClauseStack' and add the correct value to the model.
  def resolveMissingAssignments(model: Map[String,Boolean]): Map[String,Boolean] = {
    resolutionClauseStack match {
      case List() => model
      case (varName, positiveClauses) :: remainingList =>
        resolutionClauseStack = remainingList
        val strippedPositives = removeLiteralFromClauses(positiveClauses, InternalLiteral(true, varName))
        val hasFalsePositiveClause = strippedPositives.foldLeft[Boolean](false)((b, clause) => {
          b || !evaluateClause(clause, model)
        })
        resolveMissingAssignments(model + (varName -> hasFalsePositiveClause))
    }
  }

}
