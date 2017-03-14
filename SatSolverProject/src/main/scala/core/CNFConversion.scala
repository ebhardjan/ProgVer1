package core

import smtlib.parser.Terms.Term
import smtlib.theories.Core._
import util.{Combinatorial, SmtlibUtils, Utils}

object CNFConversion {
  // converts a given input formula to CNf form
  // You can assume (or indeed, use a Scala "assert" to enforce) that the given input formula satisfies util.PropositionalLogic.isPropositional(formula)
  // The output formula should satisfy util.PropositionalLogic.isCNF(..)
  def toCNF(formula: Term): Term = {
    var f = replaceImplication(formula)
    f = pushDownNegations(f)
    f = removeTopAndBottom(f)
    f = distributeConjunctionsOverDisjunctions(f)
    removeDuplicateClausesAndDuplicateLiterals(f)
  }

  /**
    * Applies step 1 from the lecture slides
    */
  def replaceImplication(formula: Term): Term = {
    formula match {
      case Not(f) => Not(replaceImplication(f))
      case Or(disjuncts@_*) => SmtlibUtils.newOrOrSingleLiteral(disjuncts.map(d => replaceImplication(d)))
      case And(conjuncts@_*) => SmtlibUtils.newAndOrSingleLiteral(conjuncts.map(c => replaceImplication(c)))
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
      case Not(True()) => False()
      case Not(False()) => True()
      case Not(Not(f)) => pushDownNegations(f)
      case Not(And(conjuncts@_*)) => SmtlibUtils.newOrOrSingleLiteral(conjuncts.map(c => pushDownNegations(Not(c))))
      case Not(Or(disjuncts@_*)) => SmtlibUtils.newAndOrSingleLiteral(disjuncts.map(d => pushDownNegations(Not(d))))
      case Not(f) => Not(pushDownNegations(f))
      case Or(disjuncts@_*) => SmtlibUtils.newOrOrSingleLiteral(disjuncts.map(d => pushDownNegations(d)))
      case And(conjuncts@_*) => SmtlibUtils.newAndOrSingleLiteral(conjuncts.map(c => pushDownNegations(c)))
      case Implies(_, _) | Equals(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found implication! Remove implications first!")
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
          SmtlibUtils.newOrOrSingleLiteral(processedDisjuncts)
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
          SmtlibUtils.newAndOrSingleLiteral(processedConjuncts)
        }
      case Implies(_, _) | Equals(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found implication! Remove implications first!")
      case _ => formula
    }
  }

  /**
    * Applies step 5 from the lecture slides
    */
  def distributeConjunctionsOverDisjunctions(formula: Term): Term = {
    formula match {
      case Not(f) => Not(distributeConjunctionsOverDisjunctions(f))
      case And(conjuncts@_*) => flatten(SmtlibUtils.newAndOrSingleLiteral(conjuncts.map(c => distributeConjunctionsOverDisjunctions(c))))
      case Or(disjuncts@_*) =>
        // recurse first
        val processedDisjuncts = disjuncts.map(d => distributeConjunctionsOverDisjunctions(d))
        // collect the disjuncts that are conjuncts
        val disjunctConjuncts = processedDisjuncts.collect({
          case And(conjuncts2@_*) => And(conjuncts2)
        }).toList
        val conjuncts = processedDisjuncts.collect({
          case And(conjuncts2@_*) => conjuncts2.toList
        }).toList
        // collect disjuncts that are not conjuncts
        val pureDisjuncts = processedDisjuncts.filter(p => !disjunctConjuncts.contains(p)).toList
        if (conjuncts.nonEmpty) {
          val conjunctsCombination = Combinatorial.calcCombinations[Term](conjuncts)
          flatten(SmtlibUtils.newAndOrSingleLiteral(conjunctsCombination.map(c => SmtlibUtils.newOrOrSingleLiteral(pureDisjuncts ++ c))))
        } else {
          flatten(SmtlibUtils.newOrOrSingleLiteral(processedDisjuncts))
        }
      case Implies(_, _) | Equals(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found implication! Remove implications first!")
      case _ => formula
    }
  }

  /**
    * helper method that flattens nested Ors and Ands
    */
  //TODO potential optimization potential, don't flatten all the way down to all leaves but only a certain number of steps?
  def flatten(formula: Term): Term = {
    formula match {
      case And(conjuncts@_*) =>
        // recurse first:
        val processedConjuncts = conjuncts.map(c => flatten(c))
        val unwrappedConjuncts = processedConjuncts.flatMap {
          case And(innerConjuncts@_*) => innerConjuncts
          case c => List(c)
        }
        SmtlibUtils.newAndOrSingleLiteral(unwrappedConjuncts)
      case Or(disjuncts@_*) =>
        // recurse first:
        val processedDisjuncts = disjuncts.map(c => flatten(c))
        val unwrappedDisjuncts = processedDisjuncts.flatMap {
          case Or(innerDisjuncts@_*) => innerDisjuncts
          case c => List(c)
        }
        SmtlibUtils.newOrOrSingleLiteral(unwrappedDisjuncts)
      case _ => formula
    }
  }

  /**
    * Applies step 6 from the lecture slides
    */
  def removeDuplicateClausesAndDuplicateLiterals(formula: Term): Term = {
    formula match {
      case And(conjuncts@_*) =>
        SmtlibUtils.newAndOrSingleLiteral(conjuncts.distinct.map(c => removeDuplicateClausesAndDuplicateLiterals(c)))
      case Or(disjuncts@_*) =>
        SmtlibUtils.newOrOrSingleLiteral(disjuncts.distinct.map(c => removeDuplicateClausesAndDuplicateLiterals(c)))
      case Implies(_, _) | Equals(_, _) =>
        throw new IllegalStateException(Utils.methodName + ": Found implication! Remove implications first!")
      case _ => formula
    }
  }

}
