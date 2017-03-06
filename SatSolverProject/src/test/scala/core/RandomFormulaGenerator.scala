package core

import smtlib.parser.Terms.Term
import util.PropositionalLogic

/**
  * Created by jan on 04.03.17.
  *
  * Creates random formulas
  *
  * @param numberOfVariables upper bound on the number variables that are used in the formula
  * @param maxChildCount     upper bound on the number of children an Or() or And() node might have
  * @param minDepth          lower bound on the depth
  * @param maxDepth          upper bound on the depth
  */
class RandomFormulaGenerator(numberOfVariables: Int,
                             maxChildCount: Int,
                             minDepth: Int,
                             maxDepth: Int) {

  val random = scala.util.Random

  if (minDepth > maxDepth) {
    throw new IllegalArgumentException("minDepth cannot be smaller than maxDepth")
  }

  def generateRandomFormula(): Term = {

    val declarationsString = CNFConversionTestUtils.smt2Declarations((0 to numberOfVariables).map(i => "p" + i))
    val formulaString = stringFormula(minDepth, maxDepth)
    val smt2String = CNFConversionTestUtils.createSmt2String(declarationsString, formulaString, getModel = true)
    val formula = CNFConversionTestUtils.smt2StringToFormula(smt2String)
    assert(PropositionalLogic.isPropositional(formula))

    formula
  }

  private[this] def stringFormula(_minDepth: Int, _maxDepth: Int): String = {
    val nextMinDepth = _minDepth - 1
    val nextMaxDepth = _maxDepth - 1

    val nextNode = randomNextNode(nextMinDepth, nextMaxDepth)

    nextNode match {
      case 0 => val coin = random.nextInt(2)
        if (coin == 0) {
          "false"
        } else {
          "true"
        }
      case 1 => val variableNumber = random.nextInt(numberOfVariables)
        "p" + variableNumber
      case 2 => "(not " + stringFormula(nextMinDepth, nextMaxDepth) + ")"
      case 3 | 4 =>
        // we need to make sure that we have at least 2 children that's what the '... - 2) + 2' is for
        val childCount = random.nextInt(maxChildCount - 2) + 2
        val children = Seq.fill(childCount)(stringFormula(nextMinDepth, nextMaxDepth))
        val foldedChildren = children.foldLeft("") { (a, b) => a + " " + b }
        nextNode match {
          case 3 => "(or " + foldedChildren + ")"
          case 4 => "(and " + foldedChildren + ")"
        }
      case 5 => "(=> " + stringFormula(nextMinDepth, nextMaxDepth) + " " + stringFormula(nextMinDepth, nextMaxDepth) + ")"
      case 6 => "(= " + stringFormula(nextMinDepth, nextMaxDepth) + " " + stringFormula(nextMinDepth, nextMaxDepth) + ")"
    }
  }

  private[this] def randomNextNode(nextMinDepth: Int, nextMaxDepth: Int): Int = {
    if (nextMaxDepth == 1) {
      random.nextInt(2)
    } else {
      if (nextMinDepth > 0) {
        random.nextInt(5) + 2
      } else {
        random.nextInt(7)
      }
    }
  }

}
