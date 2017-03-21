package core

import smtlib.parser.Terms.Term
import util.PropositionalLogic

/**
  * Created by jan on 17.03.17.
  */
class RandomCNFGenerator(numberOfVariables: Int,
                         numberOfClauses: Int,
                         minNumLiterals: Int,
                         maxNumLiterals: Int) {

  val random = scala.util.Random

  def stringFormula(): String = {
    var clauses = "(and "

    for (_ <- 1 to numberOfClauses) {
      val numLiterals = random.nextInt(maxNumLiterals - minNumLiterals + 1) + minNumLiterals
      var clause = ""
      for (_ <- 1 to numLiterals) {
        val varNumber = random.nextInt(numberOfVariables)
        val coin = random.nextBoolean()
        val literal = {
          if (!coin) {
            "(not p" + varNumber.toString + ")"
          } else {
            " p" + varNumber.toString
          }
        }
        clause += literal
      }
      clauses += "(or " + clause + ")"
    }
    clauses += ")"
    clauses
  }

  def generateRandomFormula(): Term = {
    val declarationsString = CNFConversionTestUtils.smt2Declarations((0 to numberOfVariables).map(i => "p" + i))
    val formulaString = stringFormula()
    val smt2String = CNFConversionTestUtils.createSmt2String(declarationsString, formulaString, getModel = true)
    val formula = CNFConversionTestUtils.smt2StringToFormula(smt2String)
    assert(PropositionalLogic.isPropositional(formula))
    assert(PropositionalLogic.isCNF(formula))

    formula
  }

}
