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
    * Adds all literals in clause to the model, so that they all evaluate to true.
    */
  def addLiteralsToModel(clause: InternalClause, model: Map[String, Boolean]): Map[String, Boolean] = {
    model ++ (for (d <- clause.disjuncts if {
      !model.contains(d.literal.name)
    }) yield d.literal.name -> d.literal.polarity)
  }

  /** Remove Tautologies from formula, i.e. all clauses where a literal appears positively and negatively. If the
    * resulting cnf is empty, the input was a tautology. In that case update the model by setting all the removed
    * variables to true and update the values of other disjuncts.
    */
  def removeTautologies(formula: InternalCNF, model: Map[String,Boolean]):
  Option[(InternalCNF, Map[String,Boolean])] = {
    val newConjuncts = for (c <- formula.conjuncts if {
      !SolverUtils.isTautology(c)
    }) yield c
    if (newConjuncts == formula.conjuncts) return None

    // If a variable was completely removed from the formula, add it to the model (just pick true).
    val removedVariables = for {c <- formula.conjuncts
                                d @ InternalDisjunct(InternalLiteral(polarity, name), isActive) <- c.disjuncts if {
      isActive && (c.disjuncts contains InternalDisjunct(InternalLiteral(!polarity, name), true))
    }} yield d.literal
    val newAssignments = for (l @ InternalLiteral(polarity, name) <- removedVariables if {
      polarity && !SolverUtils.containsVariable(newConjuncts, name)
    }) yield l.name -> true
    Some(InternalCNF(newConjuncts), model ++ newAssignments)
  }

  /**
    * Apply the pure literal rule. If there is a pure literal, set its value in the model and remove all clauses
    * which contain the literal from the formula.
    */
  def applyPureLiteralRule(formula: InternalCNF, model: Map[String,Boolean]) :
  Option[(InternalCNF, Map[String,Boolean])] = {
    val pureLiteral = SolverUtils.findPureLiteral(formula)
    pureLiteral match {
      case None => None
      case Some((polarity, literal)) =>
        // if there is a pure literal, remove all clauses containing it from the formula
        val newConjuncts: Set[InternalClause] = SolverUtils.takeClausesNotContainingLiteral(formula.conjuncts,
          InternalLiteral(polarity, literal))
        val newFormula: InternalCNF = InternalCNF(newConjuncts)
        val newModel: Map[String,Boolean] = model + (literal -> polarity)
        Some((newFormula, newModel))
    }
  }

  /**
    * Apply the unit propagation rule. If there is a clause with a single (active) literal, set this literal in the
    * model, remove every clause where it appears with the same polarity and remove the literal wherever it appears with
    * the opposite polarity.
    */
  def applyUnitPropagation(formula: InternalCNF, model: Map[String,Boolean]) :
  Option[(InternalCNF, Map[String,Boolean])] = {
    val unitClauses: Set[InternalClause] = for (c <- formula.conjuncts if {
      (for (d @ InternalDisjunct(_, true) <- c.disjuncts) yield d).size == 1
    }) yield c
    if (unitClauses.isEmpty) return None
    val unitClause = unitClauses.head // only do the propagation for one unit variable at a time.
    val variable: InternalDisjunct = (for (d <- unitClause.disjuncts if d.isActive) yield d).head
    val literal: InternalLiteral = variable.literal
    // add the unit clause to the model
    val newModel = model + (literal.name -> literal.polarity)
    // remove all clauses which have the variable in the same polarity
    var newConjuncts: Set[InternalClause] =
      SolverUtils.takeClausesNotContainingLiteral(formula.conjuncts, literal)
    // remove the literal with opposite polarity from clauses
    newConjuncts = SolverUtils.removeLiteralFromClauses(newConjuncts, InternalLiteral(!literal.polarity, literal.name))
    Some((InternalCNF(newConjuncts), newModel))
  }

  /**
    * Apply the resolution rule. Pick a victim literal from the formula and do resolution on it. Add the victim variable
    * together with all the clauses where it occurred positively on the resolutionClauseStack for later resolving its
    * truth value.
    */
  def applyResolutionRule(formula: InternalCNF, model: Map[String, Boolean]) :
  Option[(InternalCNF, Map[String, Boolean])] = {
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
