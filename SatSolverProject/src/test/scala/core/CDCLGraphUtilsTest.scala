package core

import org.scalatest.FunSuite
import util._

/**
  * Created by jan on 10.03.17.
  */
class CDCLGraphUtilsTest extends FunSuite {

  test("hasConflict") {
    val graph: RootNode = RootNode("rootNode", varValue = false, null)
    graph.addChild(NonDecisionLiteral("a", varValue = false, null))

    val b = NonDecisionLiteral("b", varValue = true, null)
    b.addChild(NonDecisionLiteral("a", varValue = true, null))
    graph.addChild(b)

    assert(CDCLGraphUtils.hasConflict(graph).get.equals("a"))
  }

  test("hasNoConflict") {
    val graph: RootNode = RootNode("rootNode", varValue = false, null)
    graph.addChild(NonDecisionLiteral("a", varValue = false, null))

    val b = NonDecisionLiteral("b", varValue = true, null)
    b.addChild(NonDecisionLiteral("a", varValue = false, null))
    graph.addChild(b)

    assert(CDCLGraphUtils.hasConflict(graph).isEmpty)
  }

  test("relevantDecisionLiterals") {
    val graph: RootNode = RootNode("rootNode", varValue = false, null)

    val decisionLiteral1 = DecisionLiteral("a", varValue = false, null)
    decisionLiteral1.addChild(NonDecisionLiteral("c", varValue = false, null))
    graph.addChild(decisionLiteral1)

    val decisionLiteral2 = DecisionLiteral("b", varValue = false, null)
    decisionLiteral1.addChild(decisionLiteral2)

    val decisionLiteral3 = DecisionLiteral("d", varValue = false, null)
    decisionLiteral3.addChild(NonDecisionLiteral("c", varValue = true, null))
    decisionLiteral2.addChild(decisionLiteral3)

    val relevantDecisionLiterals = CDCLGraphUtils.relevantDecisionLiterals(graph, "c")

    assert(relevantDecisionLiterals.equals(Seq(
      DecisionLiteral("a", varValue = false, null),
      DecisionLiteral("d", varValue = false, null))))
  }

  test("relevantDecisionLiteralsExampleFromSlides") {
    val graph = exampleGraphFromSlides()
    val relevantDecisionLiterals = CDCLGraphUtils.relevantDecisionLiterals(graph, "u")

    assert(relevantDecisionLiterals.equals(Seq(
      DecisionLiteral("p", varValue = true, null),
      DecisionLiteral("t", varValue = true, null))))
  }

  test("backJumpExampleFromSlides") {
    val graph = exampleGraphFromSlides()
    val conflictVar = CDCLGraphUtils.hasConflict(graph).get
    val lastNodeAfterBackjump = CDCLGraphUtils.doBackJumping(graph, conflictVar)

    val resultGraph: RootNode = RootNode("rootNode", varValue = false, null)
    val n = DecisionLiteral("n", varValue = true, null)
    val p = DecisionLiteral("p", varValue = true, null)
    val r = NonDecisionLiteral("r", varValue = true, null)
    val notT = DecisionLiteral("t", varValue = false, null)

    resultGraph.addChild(n)
    n.addChild(p)
    p.addChild(r)
    p.addChild(notT)

    assert(graph.recursiveEquals(resultGraph))
    assert(lastNodeAfterBackjump.equals(notT))
  }

  test("deleteAllNotDirectlyReachableNonDecisionLiterals") {
    val graph = RootNode("rootNode", varValue = true, null)
    val a = DecisionLiteral("a", varValue = true, null)
    val b = NonDecisionLiteral("b", varValue = true, null)
    val c = NonDecisionLiteral("c", varValue = true, null)
    val d = DecisionLiteral("d", varValue = true, null)
    val e = NonDecisionLiteral("e", varValue = true, null)

    graph.addChild(a)
    a.addChild(b)
    a.addChild(d)
    b.addChild(c)
    d.addChild(e)

    CDCLGraphUtils.deleteAllNotDirectlyReachableNonDecisionLiterals(graph)

    val expectedResult = RootNode("rootNode", varValue = true, null)
    val eA = DecisionLiteral("a", varValue = true, null)
    val eB = NonDecisionLiteral("b", varValue = true, null)
    val eD = DecisionLiteral("d", varValue = true, null)
    val eE = NonDecisionLiteral("e", varValue = true, null)
    expectedResult.addChild(eA)
    eA.addChild(eB)
    eA.addChild(eD)
    eD.addChild(eE)

    assert(graph.recursiveEquals(expectedResult))
  }

  test("getParentNodes") {
    val graph = exampleGraphFromSlides()

    val notU = NonDecisionLiteral("u", varValue = false, null)
    val p = DecisionLiteral("p", varValue = true, null)
    val t = DecisionLiteral("t", varValue = true, null)

    val parents = CDCLGraphUtils.getParentNodes(graph, notU)

    assert(parents.equals(Set(p, t)))
  }

  test("learnClause") {
    val graph = exampleGraphFromSlides()
    val conflictVar = CDCLGraphUtils.hasConflict(graph).get

    val learnedClause = CDCLGraphUtils.learnClause(graph, conflictVar)

    // note that the correctness of this depends on how we cut the graph.
    // for the current minimal implementation this is the expected outcome:
    val expectedClause = InternalClause(
      Set(
        InternalDisjunct(InternalLiteral(polarity = false, "r"), isActive = true),
        InternalDisjunct(InternalLiteral(polarity = false, "t"), isActive = true),
        InternalDisjunct(InternalLiteral(polarity = false, "p"), isActive = true)
      )
    )
    assert(learnedClause.equals(expectedClause))
  }

  def exampleGraphFromSlides(): RootNode = {
    val graph: RootNode = RootNode("rootNode", varValue = false, null)
    val n = DecisionLiteral("n", varValue = true, null)
    val p = DecisionLiteral("p", varValue = true, null)
    val r = NonDecisionLiteral("r", varValue = true, null)
    val notS = DecisionLiteral("s", varValue = false, null)
    val q = NonDecisionLiteral("q", varValue = true, null)
    val t = DecisionLiteral("t", varValue = true, null)
    val u = NonDecisionLiteral("u", varValue = true, null)
    val notU = NonDecisionLiteral("u", varValue = false, null)

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
