package core

import smtlib.parser.Terms.{QualifiedIdentifier, SimpleIdentifier, Term}
import smtlib.theories.Core._

object CNFConversion {
  // converts a given input formula to CNf form
  // You can assume (or indeed, use a Scala "assert" to enforce) that the given input formula satisfies util.PropositionalLogic.isPropositional(formula)
  // The output formula should satisfy util.PropositionalLogic.isCNF(..)
  def toCNF(formula : Term) : Term = ??? // the ??? syntax allows the declaration of an unimplemented method (an exception will be thrown at runtime if the ??? term is evaluated)
}
