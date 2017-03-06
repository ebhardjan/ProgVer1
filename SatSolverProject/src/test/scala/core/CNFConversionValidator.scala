package core

import smtlib.parser.Terms.Term
import smtlib.theories.Core.False
import util.PropositionalLogic

/**
  * Created by jan on 06.03.17.
  */
object CNFConversionValidator {

  /**
    * converts a given formula into cnf and checks if the conversion was
    *
    * @param formula any propositional logic formula
    * @return whether the formula was converted correctly or not
    */
  def convertToCnfAndValidate(formula: Term): Boolean = {
    var cnf = False()

    // convert to cnf and return false in case an error occurred
    cnf = CNFConversion.toCNF(formula)

    // is the formula now really in cnf?
    if (!PropositionalLogic.isCNF(cnf)) {
      println("Formula not in CNF!")
      return false
    }

    // is the formula equivalent to the original one?
    if (!CNFConversionTestUtils.formulasEqual(formula, cnf)) {
      println("Formula in CNF but transformed formula is not equivalent to the original one!")
      return false
    }
    true
  }

}
