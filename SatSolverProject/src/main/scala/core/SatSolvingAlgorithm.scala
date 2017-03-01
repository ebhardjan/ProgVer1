package core

import smtlib.parser.Terms.Term

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
