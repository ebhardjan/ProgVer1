package core

import org.scalatest.FunSuite
import util.InternalLiteral

/**
  * Created by jan on 10.03.17.
  */
class CDCLSolverUnitTest extends FunSuite {

  test("hasConflict") {
    val graph: RootNode = RootNode("rootNode", _varValue = false, null)
    graph.addChild(NonDecisionLiteral("a", _varValue = false, null))
    graph.addChild(NonDecisionLiteral("b", _varValue = true, null))
    graph.children(1).addChild(NonDecisionLiteral("a", _varValue = true, null))

    assert(CDCLSolver.hasConflict(graph))
  }

  test("hasNoConflict") {
    val graph: RootNode = RootNode("rootNode", _varValue = false, null)
    graph.addChild(NonDecisionLiteral("a", _varValue = false, null))
    graph.addChild(NonDecisionLiteral("b", _varValue = true, null))
    graph.children(1).addChild(NonDecisionLiteral("a", _varValue = false, null))

    assert(!CDCLSolver.hasConflict(graph))
  }

  test("relevantDecisionLiterals") {
    val graph: RootNode = RootNode("rootNode", _varValue = false, null)

    val decisionLiteral1 = DecisionLiteral("a", _varValue = false, null)
    decisionLiteral1.addChild(NonDecisionLiteral("c", _varValue = false, null))
    graph.addChild(decisionLiteral1)

    val decisionLiteral2 = DecisionLiteral("b", _varValue = false, null)
    decisionLiteral1.addChild(decisionLiteral2)

    val decisionLiteral3 = DecisionLiteral("d", _varValue = false, null)
    decisionLiteral3.addChild(NonDecisionLiteral("c", _varValue = true, null))
    decisionLiteral2.addChild(decisionLiteral3)

    val relevantDecisionLiterals = CDCLSolver.relevantDecisionLiterals(graph, "c")

    assert(relevantDecisionLiterals.equals(Seq(
      DecisionLiteral("a", _varValue = false, null),
      DecisionLiteral("d", _varValue = false, null))))
  }

}
