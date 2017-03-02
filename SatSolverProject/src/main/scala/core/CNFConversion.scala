package core

import smtlib.parser.Terms.Term
import smtlib.theories.Core._
import util.Combinatorial
import util.Utils

object CNFConversion {
  // converts a given input formula to CNf form
  // You can assume (or indeed, use a Scala "assert" to enforce) that the given input formula satisfies util.PropositionalLogic.isPropositional(formula)
  // The output formula should satisfy util.PropositionalLogic.isCNF(..)
  def toCNF(formula: Term): Term = {
    var f = replaceImplication(formula)
    f = pushDownNegations(f)
    f = removeTopAndBottom(f)
    f = distributeConjunctionsOverDisjunctions(f)
    f = removeDuplicateClausesAndDuplicateLiterals(f)
    removeTopAndBottom(f) // TODO: not sure if this is necessary
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
      case Not(f) => Not(pushDownNegations(f))
      case Or(disjuncts@_*) => Or(disjuncts.map(d => pushDownNegations(d)))
      case And(conjuncts@_*) => And(conjuncts.map(c => pushDownNegations(c)))
      case Implies(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found implication! Remove implications first!")
      case Equals(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found double implication! Remove implications first!")
      case _ => formula
    }
  }

  /**
    * Applies step 4 from the lecture slides
    */
  def removeTopAndBottom(formula: Term): Term = {
    formula match {
      case Not(f) => Not(removeTopAndBottom(f))
      case Or(disjuncts@_*) =>
        // recurse first
        val processedDisjuncts = disjuncts.map(d => removeTopAndBottom(d))
          .filter({
            // we can already drop all the False() elements
            case False() => false
            case _ => true
          })
        // check if the sequence of disjuncts contains any disjuncts that are True()
        val trueDisjunctExists = processedDisjuncts.collect({ case True() => True() }).nonEmpty
        // if there are any disjuncts that are True, we can replace the whole clause with true
        if (trueDisjunctExists) {
          True()
        } else {
          // otherwise we do nothing
          newOrOrSingleLiteral(processedDisjuncts)
        }
      case And(conjuncts@_*) =>
        // recurse first
        val processedConjuncts = conjuncts.map(c => removeTopAndBottom(c))
          .filter({
            // we can already drop all the True() elements
            case True() => false
            case _ => true
          })
        // check if the sequence of conjuncts contains any disjuncts that are True()
        val falseConjunctExists = processedConjuncts.collect({ case False() => False() }).nonEmpty
        // if there are any conjuncts that are False, we can replace the whole clause with false
        if (falseConjunctExists) {
          False()
        } else {
          // otherwise we do nothing
          newAndOrSingleLiteral(processedConjuncts)
        }
      case Implies(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found implication! Remove implications first!")
      case Equals(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found double implication! Remove implications first!")
      case _ => formula
    }
  }

  /**
    * Applies step 5 from the lecture slides
    */
  def distributeConjunctionsOverDisjunctions(formula: Term): Term = {
    formula match {
      case Not(f) => Not(distributeConjunctionsOverDisjunctions(f))
        //TODO: flatten here?
      case And(conjuncts@_*) => And(conjuncts.map(c => distributeConjunctionsOverDisjunctions(c)))
      case Or(disjuncts@_*) =>
        // recurse first
        val processedDisjuncts = disjuncts.map(d => distributeConjunctionsOverDisjunctions(d))
        // collect the disjuncts that are conjuncts
        val disjunctConjuncts = disjuncts.collect({
          case And(conjuncts2@_*) => And(conjuncts2)
        }).toList
        val conjuncts = disjuncts.collect({
          case And(conjuncts2@_*) => conjuncts2.toList
        }).toList
        // collect disjuncts that are not conjuncts
        val pureDisjuncts = disjuncts.filter(p => !disjunctConjuncts.contains(p)).toList
        if (conjuncts.nonEmpty && pureDisjuncts.nonEmpty) {
          val conjunctsCombination = Combinatorial.calcCombinations[Term](conjuncts)
          // TODO: problem, here we start nesting Ors! -> general concern: whenever we change the contents of an Or or an And we should check the new contents and try to flatten it!
          And(conjunctsCombination.map(c => Or(pureDisjuncts ++ c)))
        } else {
          // TODO flatten also here, right?
          Or(processedDisjuncts)
        }
      case Implies(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found implication! Remove implications first!")
      case Equals(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found double implication! Remove implications first!")
      case _ => formula
    }
  }

  /**
    * Applies step 6 from the lecture slides
    */
  def removeDuplicateClausesAndDuplicateLiterals(formula: Term): Term = {
    formula match {
      case And(conjuncts@_*) =>
        newAndOrSingleLiteral(conjuncts.distinct.map(c => removeDuplicateClausesAndDuplicateLiterals(c)))
      case Or(disjuncts@_*) =>
        newOrOrSingleLiteral(disjuncts.distinct.map(c => removeDuplicateClausesAndDuplicateLiterals(c)))
      case Implies(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found implication! Remove implications first!")
      case Equals(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found double implication! Remove implications first!")
      case _ => formula
    }
  }

  /**
    * either return an And() in case conjuncts has more than 1 elements, otherwise just return the one element
    */
  def newAndOrSingleLiteral(conjuncts: Seq[Term]): Term = {
    if (conjuncts.length > 1) {
      And(conjuncts)
    } else {
      conjuncts.head
    }
  }

  /**
    * either return an Or() in case disjuncts has more than 1 elements, otherwise just return the one element
    */
  def newOrOrSingleLiteral(disjuncts: Seq[Term]): Term = {
    if (disjuncts.length > 1) {
      Or(disjuncts)
    } else {
      disjuncts.head
    }
  }

}
