package core

import smtlib.parser.Terms.Term
import smtlib.theories.Core._
import util.Combinatorial

object CNFConversion {
  // converts a given input formula to CNf form
  // You can assume (or indeed, use a Scala "assert" to enforce) that the given input formula satisfies util.PropositionalLogic.isPropositional(formula)
  // The output formula should satisfy util.PropositionalLogic.isCNF(..)
  def toCNF(formula: Term): Term = {
    val f = replaceImplication(formula)
    f
  }

  /**
    * Applies step 1 from the lecture slides
    */
  def replaceImplication(formula: Term): Term = {
    formula match {
      case Not(f) => Not(replaceImplication(f))
      case Or(disjuncts@_*) => Or(disjuncts.map(d => replaceImplication(d)))
      case And(conjuncts@_*) => And(conjuncts.map(c => replaceImplication(c)))
      case Implies(f, g) => Or(Not(replaceImplication(f)), replaceImplication(g))
      case Equals(f, g) =>
        // let's evaluate the subexpressions first, otherwise they get evaluated twice
        val f_ = replaceImplication(f)
        val g_ = replaceImplication(g)
        And(Or(Not(f_), g_), Or(f_, Not(g_)))
      case _ => formula
    }
  }

  /**
    * Applies step 2 and 3 from the lecture slides
    */
  def pushDownNegations(formula: Term): Term = {
    formula match {
      case Not(Not(f)) => pushDownNegations(f)
      case Not(And(f, g)) => Or(pushDownNegations(Not(f)), pushDownNegations(Not(g)))
      case Not(Or(f, g)) => And(pushDownNegations(Not(f)), pushDownNegations(Not(g)))
      case Not(f) => Not(pushDownNegations(f)) // we just recurse to be safe,
      // I think in all cases where we end up here we could just return f
      case Or(disjuncts@_*) => Or(disjuncts.map(d => pushDownNegations(d)))
      case And(conjuncts@_*) => And(conjuncts.map(c => pushDownNegations(c)))
      case Implies(_, _) =>
        throw new IllegalStateException("Push negations down: Found implication! Remove implications first!")
      case Equals(_, _) =>
        throw new IllegalStateException("Push negations down: Found double implication! Remove implications first!")
      case _ => formula
    }
  }

  /**
    * Applies step 4 from the lecture slides
    */
  def removeTopAndBottom(formula: Term): Term = {
    formula match {
      case Not(f) => Not(removeTopAndBottom(f))
      case Or(disjuncts@_*) => Or(disjuncts.map(d => removeTopAndBottom(d)))
      case And(conjuncts@_*) => And(conjuncts.map(c => removeTopAndBottom(c)))
      case Implies(_, _) =>
        throw new IllegalStateException("Push negations down: Found implication! Remove implications first!")
      case Equals(_, _) =>
        throw new IllegalStateException("Push negations down: Found double implication! Remove implications first!")
      //TODO: fancy special case here
      case _ => formula
    }
  }

  /**
    * Applies step 5 from the lecture slides
    */
  def distributeConjunctionsOverDisjunctions(formula: Term): Term = {
    formula match {
      case Not(f) => Not(distributeConjunctionsOverDisjunctions(f))
      case And(conjuncts@_*) => And(conjuncts.map(c => distributeConjunctionsOverDisjunctions(c)))
      case Or(disjuncts@_*) =>
        //TODO prettify this!
        // collect the disjuncts that are conjuncts
        val disjunctConjuncts = disjuncts.collect({
          case And(conjuncts2@_*) => And(conjuncts2)
        }).toList
        val conjuncts = disjuncts.collect({
          case And(conjuncts2@_*) => conjuncts2.toList
        }).toList
        // collect disjuncts that are not conjuncts
        val pureDisjuncts = disjuncts.filter(p => !disjunctConjuncts.contains(p)).toList
        if (conjuncts != Nil && pureDisjuncts != Nil) {
          val conjunctsCombination = Combinatorial.calcCombinations[Term](conjuncts)
          And(conjunctsCombination.map(c => Or(pureDisjuncts ++ c)))
        } else {
          Or(disjuncts.map(d => distributeConjunctionsOverDisjunctions(d)))
        }
      case Implies(_, _) =>
        throw new IllegalStateException("Push negations down: Found implication! Remove implications first!")
      case Equals(_, _) =>
        throw new IllegalStateException("Push negations down: Found double implication! Remove implications first!")
      case _ => formula
    }
  }

  /**
    * Applies step 6 from the lecture slides
    */
  def removeDuplicateClauses(formula: Term): Term = {
    formula match {
      case And(conjuncts@_*) =>
        //TODO remove duplicate clauses here
        And(conjuncts)
      //TODO remove duplicate literals in all the disjuncts here
    }
  }

}
