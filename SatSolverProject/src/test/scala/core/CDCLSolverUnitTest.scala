package core

import org.scalatest.FunSuite

/**
  * Created by jan on 10.03.17.
  */
class CDCLSolverUnitTest extends FunSuite {

  test("hasConflict") {
    val graph: RootNode = RootNode("rootNode", _varValue = false, null)
    graph.addChild(NonDecisionLiteral("a", _varValue = false, null))

    val b = NonDecisionLiteral("b", _varValue = true, null)
    b.addChild(NonDecisionLiteral("a", _varValue = true, null))
    graph.addChild(b)

    assert(CDCLSolver.hasConflict(graph).get.equals("a"))
  }

  test("hasNoConflict") {
    val graph: RootNode = RootNode("rootNode", _varValue = false, null)
    graph.addChild(NonDecisionLiteral("a", _varValue = false, null))

    val b = NonDecisionLiteral("b", _varValue = true, null)
    b.addChild(NonDecisionLiteral("a", _varValue = false, null))
    graph.addChild(b)

    assert(CDCLSolver.hasConflict(graph).isEmpty)
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

  test("relevantDecisionLiteralsExampleFromSlides") {
    val graph = exampleGraphFromSlides()
    val relevantDecisionLiterals = CDCLSolver.relevantDecisionLiterals(graph, "u")

    assert(relevantDecisionLiterals.equals(Seq(
      DecisionLiteral("p", _varValue = true, null),
      DecisionLiteral("t", _varValue = true, null))))
  }

  test("backJumpExampleFromSlides") {
    val graph = exampleGraphFromSlides()
    val conflictVar = CDCLSolver.hasConflict(graph).get
    val lastNodeAfterBackjump = CDCLSolver.doBackJumping(graph, conflictVar)

    val resultGraph: RootNode = RootNode("rootNode", _varValue = false, null)
    val n = DecisionLiteral("n", _varValue = true, null)
    val p = DecisionLiteral("p", _varValue = true, null)
    val r = NonDecisionLiteral("r", _varValue = true, null)
    val notT = DecisionLiteral("t", _varValue = false, null)

    resultGraph.addChild(n)
    n.addChild(p)
    p.addChild(r)
    p.addChild(notT)

    assert(graph.equals(resultGraph))
    assert(lastNodeAfterBackjump.equals(notT))
  }

  test("deleteAllNotDirectlyReachableNonDecisionLiterals") {
    val graph = RootNode("rootNode", _varValue = true, null)
    val a = DecisionLiteral("a", _varValue = true, null)
    val b = NonDecisionLiteral("b", _varValue = true, null)
    val c = NonDecisionLiteral("c", _varValue = true, null)
    val d = DecisionLiteral("d", _varValue = true, null)
    val e = NonDecisionLiteral("e", _varValue = true, null)

    graph.addChild(a)
    a.addChild(b)
    a.addChild(d)
    b.addChild(c)
    d.addChild(e)

    CDCLSolver.deleteAllNotDirectlyReachableNonDecisionLiterals(graph)

    val expectedResult = RootNode("rootNode", _varValue = true, null)
    val eA = DecisionLiteral("a", _varValue = true, null)
    val eB = NonDecisionLiteral("b", _varValue = true, null)
    val eD = DecisionLiteral("d", _varValue = true, null)
    val eE = NonDecisionLiteral("e", _varValue = true, null)
    expectedResult.addChild(eA)
    eA.addChild(eB)
    eA.addChild(eD)
    eD.addChild(eE)

    assert(graph.equals(expectedResult))
  }

  def exampleGraphFromSlides(): RootNode = {
    val graph: RootNode = RootNode("rootNode", _varValue = false, null)
    val n = DecisionLiteral("n", _varValue = true, null)
    val p = DecisionLiteral("p", _varValue = true, null)
    val r = NonDecisionLiteral("r", _varValue = true, null)
    val notS = DecisionLiteral("s", _varValue = false, null)
    val q = NonDecisionLiteral("q", _varValue = true, null)
    val t = DecisionLiteral("t", _varValue = true, null)
    val u = NonDecisionLiteral("u", _varValue = true, null)
    val notU = NonDecisionLiteral("u", _varValue = false, null)

    graph.addChild(n)
    n.addChild(p)
    p.addChild(r)
    p.addChild(notU)
    p.addChild(notS)
    notS.addChild(q)
    notS.addChild(t)
    r.addChild(u)
    t.addChild(u)
    t.addChild(notU)

    graph
  }

}
