package core

import smtlib.parser.Terms.{QualifiedIdentifier, SimpleIdentifier, Term}
import smtlib.theories.Core._

object CNFConversion {
  // converts a given input formula to CNf form
  // You can assume (or indeed, use a Scala "assert" to enforce) that the given input formula satisfies util.PropositionalLogic.isPropositional(formula)
  // The output formula should satisfy util.PropositionalLogic.isCNF(..)
  def toCNF(formula : Term) : Term = {
    val f = replaceImplication(formula)
    f
  }

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

}
