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
      var changed = false
      while (!cnfRep.containsEmptyClause() && cnfRep.conjuncts.nonEmpty) {
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

        // TODO: implement resolution rule
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
  def applyPureLiteralRule(formula : InternalCNF, model : Map[String,Boolean]) :
  (InternalCNF, Map[String,Boolean], Boolean) = {
    val pureLiteral = findPureLiteral(formula)
    pureLiteral match {
      case None => (formula, model, false)
      case Some((literal, value)) =>
        // if there is a pure literal, remove all clauses containing it from the formula
        val newConjuncts : Set[InternalClause] = for (c <- formula.conjuncts if {
          (for (d @ InternalDisjunct(InternalLiteral(_, varName), _) <- c.disjuncts if varName == literal)
            yield d).isEmpty
        }) yield c
        val newFormula : InternalCNF = InternalCNF(newConjuncts)
        val newModel : Map[String,Boolean] = model + (literal -> value)
        (newFormula, newModel, true)
    }
  }

  // Apply the unit propagation rule. If there is a clause with a single (active) literal, set this literal in the
  // model, remove every clause where it appears with the same polarity and remove (deactivate) the literal wherever
  // it appears with the opposite polarity.
  def applyUnitPropagation(formula : InternalCNF, model : Map[String,Boolean]) :
  (InternalCNF, Map[String,Boolean], Boolean) = {
    val unitClauses : Set[InternalClause] = for (c <- formula.conjuncts if {
      (for (d @ InternalDisjunct(_, true) <- c.disjuncts) yield d).size == 1
    }) yield c
    if (unitClauses.isEmpty) return (formula, model, false)
    var newFormula : InternalCNF = formula
    var newModel : Map[String,Boolean] = model
    for (unitClause <- unitClauses) {
      val variable : InternalDisjunct = (for (d <- unitClause.disjuncts if d.isActive) yield d).head
      // add the unit clause to the model
      newModel = newModel + (variable.literal.name -> variable.literal.polarity)
      // remove all clauses which have the variable in the same polarity
      val newConjuncts : Set[InternalClause] = for (c <- newFormula.conjuncts if {
        (for (d @ InternalDisjunct(InternalLiteral(pol, varName),_) <- c.disjuncts if {
          varName == variable.literal.name && pol == variable.literal.polarity
        }) yield d).isEmpty
      }) yield c
      // remove (deactivate) the literal with opposite polarity from clauses
      for (c <- newConjuncts) {
        for (d <- c.disjuncts) {
          if (d.literal.name == variable.literal.name) {
            d.isActive = false
          }
        }
      }
      newFormula = InternalCNF(newConjuncts)
    }
    (newFormula, newModel, true)
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
