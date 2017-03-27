package core

import org.scalatest.FunSuite
import util._

/**
  * Created by jan on 10.03.17.
  */
class CDCLGraphUtilsTest extends FunSuite {

  test("hasConflict") {
    val graph: RootNode = RootNode("rootNode", varValue = false, null)

    val nA = NonDecisionLiteral("a", varValue = false, null)
    graph.addNode(nA)
    graph.addChild(nA)

    val a = NonDecisionLiteral("a", varValue = true, null)
    graph.addNode(a)

    val b = NonDecisionLiteral("b", varValue = true, null)
    graph.addNode(b)

    b.addChild(a)
    graph.addChild(b)

    assert(CDCLGraphUtils.hasConflict(graph).get.equals("a"))
  }

  test("hasNoConflict") {
    val graph: RootNode = RootNode("rootNode", varValue = false, null)
    val nA1 = NonDecisionLiteral("a", varValue = false, null)
    graph.addNode(nA1)
    graph.addChild(nA1)

    val b = NonDecisionLiteral("b", varValue = true, null)
    graph.addNode(b)

    val nA2 = NonDecisionLiteral("a", varValue = false, null)
    graph.addNode(nA2)

    b.addChild(nA2)
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
    val (parentDecisionLiteral, varName, varValue) = CDCLGraphUtils.doBackJumping(graph, conflictVar)

    val resultGraph: RootNode = RootNode("rootNode", varValue = false, null)
    val n = DecisionLiteral("n", varValue = true, null)
    val p = DecisionLiteral("p", varValue = true, null)
    val r = NonDecisionLiteral("r", varValue = true, null)
    val notT = DecisionLiteral("t", varValue = false, null)

    resultGraph.addChild(n)
    n.addChild(p)
    p.addChild(r)
    p.addChild(notT)

    parentDecisionLiteral.addChild(DecisionLiteral(varName, varValue, null))
    assert(graph.recursiveEquals(resultGraph))
    assert(varName.equals(notT.varName))
    assert(varValue.equals(notT.varValue))
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

  test("deleteDecisionLiteralStartingWithChildOf") {
    val root = RootNode("rootNode", varValue = false, null)
    val nP0 = DecisionLiteral("p0", varValue = false, null)
    val nP2 = DecisionLiteral("p2", varValue = false, null)
    val nP3 = NonDecisionLiteral("p3", varValue = false, null)
    val nP1 = NonDecisionLiteral("p1", varValue = false, null)
    val p5 = NonDecisionLiteral("p5", varValue = true, null)
    val p4 = NonDecisionLiteral("p4", varValue = true, null)
    val nP4 = NonDecisionLiteral("p4", varValue = false, null)
    root.addChild(nP0)

    nP0.addChild(nP3)
    nP0.addChild(nP1)
    nP0.addChild(p5)
    nP0.addChild(p4)
    nP0.addChild(nP2)
    nP0.addDecisionImplication(nP3)
    nP0.addDecisionImplication(nP1)

    nP3.addChild(nP1)
    nP3.addChild(p5)

    p5.addChild(p4)

    nP2.addChild(p5)
    nP2.addChild(nP4)

    nP2.addDecisionImplication(p5)
    nP2.addDecisionImplication(p4)
    nP2.addDecisionImplication(nP4)

    CDCLGraphUtils.deleteAllDecisionLiteralsStartingWithChildOf(root, nP0)

    val expected = RootNode("rootNode", varValue = false, null)
    val eNP0 = DecisionLiteral("p0", varValue = false, null)
    val eNP3 = NonDecisionLiteral("p3", varValue = false, null)
    val eNP1 = NonDecisionLiteral("p1", varValue = false, null)
    expected.addChild(eNP0)
    eNP0.addChild(eNP3)
    eNP0.addChild(eNP1)
    eNP3.addChild(eNP1)

    assert(root.recursiveEquals(expected))
  }

  def exampleGraphFromSlides(): RootNode = {
    val graph: RootNode = RootNode("rootNode", varValue = false, null)
    val n = DecisionLiteral("n", varValue = true, null)
    val p = DecisionLiteral("p", varValue = true, null)
    val r = NonDecisionLiteral("r", varValue = true, p)
    val notS = DecisionLiteral("s", varValue = false, null)
    val q = NonDecisionLiteral("q", varValue = true, notS)
    val t = DecisionLiteral("t", varValue = true, null)
    val u = NonDecisionLiteral("u", varValue = true, t)
    val notU = NonDecisionLiteral("u", varValue = false, t)

    graph.addNode(n)
    graph.addNode(p)
    graph.addNode(r)
    graph.addNode(notS)
    graph.addNode(q)
    graph.addNode(t)
    graph.addNode(u)
    graph.addNode(notU)

    p.addDecisionImplication(r)
    notS.addDecisionImplication(q)
    t.addDecisionImplication(u)
    t.addDecisionImplication(notU)

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
