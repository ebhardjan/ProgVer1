package core

import smtlib.parser.Terms.Term
import util._

import scala.collection.immutable.Set

// Scala allows declaring multiple classes/objects/... in a single file.
// It is common practice to group logically connected (often small) classes etc. this way.

trait SATSolvingAlgorithm {
  // perform satisfiability checking on the given input formula (which can be assumed to be in CNF format; i.e. PropositionalLogic.isCNF(formula) should be true
  // returns None if the formula is unsatisfiable
  // returns Some(m) if the formula is satisfiable, where m should be a Map instance representing a model (mapping the variables to true/false Boolean values
  def checkSAT(formula : Term) : Option[Map[String,Boolean]]

  // converts the result from a call to "checkSAT" above into a String representation (according to the SMT-LIB standard)
  def outputResult(res : Option[Map[String,Boolean]]) : String =
    // Pattern match on "res"
    res match {
      case None => "unsat"
      case Some(m) =>
        // No need to surround the body of a pattern matching case with curly braces (it's always a code block)
        m.keySet.foldLeft("sat\n(model\n")((s, key) => s"$s  (define-fun $key () Bool\n    ${m(key)})\n") + ")"
    }

  /**
    * Remove Tautologies from formula, i.e. all clauses where a literal appears positively and negatively. Return None
    * if the formula remains unchanged, otherwise return the new formula.
    */
  def removeTautologies(formula: InternalCNF, model: Map[String,Boolean]): Option[InternalCNF] = {
    val newConjuncts = for (c <- formula.conjuncts if {
      !SolverUtils.isTautology(c)
    }) yield c
    if (newConjuncts == formula.conjuncts) {
      None
    } else {
      Some(InternalCNF(newConjuncts))
    }
  }

  /**
    * Apply the pure literal rule. If there is a pure literal, remove all clauses which contain the literal from the
    * formula, return the new formula and the new assignment. Otherwise return None.
    */
  def applyPureLiteralRule(formula: InternalCNF, model: Map[String,Boolean])
  : Option[(InternalCNF, (String, Boolean))] = {
    val pureLiteral = SolverUtils.findPureLiteral(formula)
    pureLiteral match {
      case None => None
      case Some((polarity, literal)) =>
        // if there is a pure literal, remove all clauses containing it from the formula
        val newConjuncts: Set[InternalClause] = SolverUtils.takeClausesNotContainingLiteral(formula.conjuncts,
          InternalLiteral(polarity, literal))
        val newFormula: InternalCNF = InternalCNF(newConjuncts)
        Some((newFormula, literal -> polarity))
    }
  }

  /**
    * Apply the unit propagation rule. If there is a clause with a single (active) literal, remove every clause where it
    * appears with the same polarity and remove the literal wherever it appears with the opposite polarity.
    * Return the new formula and the new assignment for the model. If there is unit clause return None
    */
  def applyUnitPropagation(formula: InternalCNF)
  : Option[(InternalCNF, (String, Boolean), InternalClause)] = {
    val unitClauses: Set[InternalClause] = for (c <- formula.conjuncts if {
      (for (d @ InternalDisjunct(_, true) <- c.disjuncts) yield d).size == 1
    }) yield c

    if (unitClauses.isEmpty)
      return None

    applyUnitPropagationOn(formula, unitClauses.head)
  }

  def applyUnitPropagationOn(formula: InternalCNF, unitClause: InternalClause)
  : Option[(InternalCNF, (String, Boolean), InternalClause)] = {
    val variable: InternalDisjunct = (for (d <- unitClause.disjuncts if d.isActive) yield d).head
    val literal: InternalLiteral = variable.literal
    // remove all clauses which have the variable in the same polarity
    var newConjuncts: Set[InternalClause] =
      SolverUtils.takeClausesNotContainingLiteral(formula.conjuncts, literal)
    // remove the literal with opposite polarity from clauses
    newConjuncts = SolverUtils.removeLiteralFromClauses(newConjuncts, InternalLiteral(!literal.polarity, literal.name))
    Some((InternalCNF(newConjuncts), literal.name -> literal.polarity, unitClause))
  }
}

// You may find the following code stubs useful (you could also copy them to other files)

// To define a SATSolving algorithm as a singleton object, use something like this:
object DummySATSolving extends SATSolvingAlgorithm {
  override def checkSAT(formula : Term) = ???
}

// To define a SATSolving algorithm as a class, use something like this:
class DummySATSolvingClass extends SATSolvingAlgorithm {
  override def checkSAT(formula : Term) = ???
}
