package core

import smtlib.parser.Terms.Term
import util._

/**
  * Created by jan on 09.03.17.
  */
class CDCLSolver extends SATSolvingAlgorithm {

  var rootChoices: Set[InternalLiteral] = Set()
  var graph: RootNode = _

  /**
    * Applies the CDCL algorithm to the given formula. Returns None in case of unsat, otherwise the model mapping
    * variables to booleans.
    */
  override def checkSAT(formula: Term): Option[Map[String, Boolean]] = {
    val cnfRep: InternalCNF = CNFRepresentation.convertCNFToInternal(formula)

    // case where we have InternalConjunct(InternalDisjunct()) handle this empty case separately.
    if (cnfRep.conjuncts.size == 1 && cnfRep.conjuncts.head.disjuncts.isEmpty) {
      return None
    }

    graph = RootNode("root", varValue = true, cnfRep)
    if (runCDCL(graph, false)) {
      Some(SolverUtils.setAllRemainingVariables(cnfRep, graph.toModel))
    } else {
      None
    }
  }

  def digestUnitPropagation(lastNode: ADecisionLiteral, r: (String, Boolean), unitClause: InternalClause): Unit = {
    val newNode = NonDecisionLiteral(r._1, r._2, lastNode)

    // add the node to the implications of the last decision literal
    lastNode.addDecisionImplication(newNode)

    // find all the nodes that were previously in the literal, take their negation and add edges from them to the
    // new node in case they exist.
    val previouslyRemovedLiterals = unitClause.disjuncts.filter(d => !d.isActive)
    if (previouslyRemovedLiterals.isEmpty) {
      graph.addChild(newNode)
    } else {
      previouslyRemovedLiterals
        .map(l => l.literal)
        .map(l => CDCLGraphUtils.findNode(graph, InternalLiteral(!l.polarity, l.name)))
        .collect({ case Some(n) => n })
        .foreach(n => n.addChild(newNode))
    }
  }

  /**
    * Does tautology removal, unit propagation and pure literal elimination until either a conflict is reached, or we
    * cannot do anything anymore and need to guess another variable.
    *
    * @param lastNode the last added node, this is where the children will be added
    */
  def runToComplete(lastNode: ADecisionLiteral): Unit = {
    if (CDCLGraphUtils.hasConflict(graph).isDefined) {
      return
    }
    if (lastNode.formula.conjuncts.isEmpty) {
      return
    }
    removeTautologies(lastNode.formula, graph.toModel) match {
      case Some(f) =>
        lastNode.formula = f
        runToComplete(lastNode)
        return
      case None =>
    }
    applyUnitPropagation(lastNode.formula) match {
      case Some((f, r, unitClause)) =>
        var formula = f
        // in case we have empty disjuncts now, also apply unit propagation on the negation of r
        if (f.conjuncts.exists(c => !c.disjuncts.exists(d => d.isActive))) {
          // take the first clause where the negation of r appears, set the negation to active and propagate
          val emptyClauses = f.conjuncts
            .filter(c => !c.disjuncts.exists(d => d.isActive))
            .filter(c => c.disjuncts.exists(d => d.literal.name.equals(r._1) && d.literal.polarity.equals(!r._2)))
          val emptyClause = emptyClauses.head // TODO: sure it's enough to only bother about head?
          val newClause = InternalClause(emptyClause.disjuncts.map(d =>
            if (d.literal.name.equals(r._1) && d.literal.polarity.equals(!r._2)) {
              InternalDisjunct(InternalLiteral(!r._2, r._1), isActive = true)
            } else {
              d
            }))
          formula = InternalCNF(f.conjuncts.filter(c => !c.equals(emptyClause)) + newClause)
          val res = applyUnitPropagationOn(formula, newClause).get
          digestUnitPropagation(lastNode, res._2, res._3)
          // we need to pass on the new formula, so that we can later pick the correct next decision literal!
          formula = res._1
        }
        digestUnitPropagation(lastNode, r, unitClause)
        lastNode.formula = formula
        runToComplete(lastNode)
      case None =>
    }
    /* TODO should we bother about this one?
    applyPureLiteralRule(lastNode.formula, root.toModel) match {
      case Some((f, r)) =>
        val newNode = NonDecisionLiteral(r._1, r._2, f)
        lastNode.addChild(newNode)
        lastNode.formula = f
        runToComplete(root, lastNode)
      case None =>
      */
  }

  /**
    * Recursively apply the CDCL steps until the SAT question is answered.
    */
  def runCDCL(lastNode: ADecisionLiteral, unsatWithNextConflict: Boolean): Boolean = {
    runToComplete(lastNode)
    val conflictVarName = CDCLGraphUtils.hasConflict(graph)
    conflictVarName match {
      case Some(name) =>

        // conflict without even making a choice!
        if (!graph.children.exists({case _: DecisionLiteral => true case _ => false})) {
          return false
        }

        if (unsatWithNextConflict) {
          return false
        }

        val learnedClause = CDCLGraphUtils.learnClause(graph, name)

        val (newLastNodeParent, newVarName, newVarValue) = CDCLGraphUtils.doBackJumping(graph, name)
        CDCLGraphUtils.addClauseToAllFormulas(graph, learnedClause)

        // just for the new clause that we just learned
        runToComplete(newLastNodeParent)

        val newLastNode = DecisionLiteral(newVarName, newVarValue, newLastNodeParent.formula)
        newLastNodeParent.addChild(newLastNode)

        // TODO remove this duplicate code!
        // TODO: no nodes are introduced here! !b should have an incoming edge from a for instance
        val updatedClauses =
          SolverUtils.takeClausesNotContainingLiteral(newLastNode.formula.conjuncts,
            InternalLiteral(newLastNode.varValue, newLastNode.varName))
        // remove the negation of the literal from all clauses
        val updatedFormula =
          InternalCNF(SolverUtils.removeLiteralFromClauses(updatedClauses,
            InternalLiteral(!newLastNode.varValue, newLastNode.varName)))

        newLastNode.formula = updatedFormula
        val stopOnNextConflict = addToRootChoicesIfNecessary(graph, newLastNode, InternalLiteral(newLastNode.varValue, newLastNode.varName))
        runCDCL(newLastNode, stopOnNextConflict)
      case None =>
        if (lastNode.formula.conjuncts.isEmpty) {
          // if we processed the formula completely and there is no conflict, we are done and return SAT
          return true
        }

        val decisionLiteral = pickDecisionLiteral(lastNode.formula)


        val updatedClauses =
          SolverUtils.takeClausesNotContainingLiteral(lastNode.formula.conjuncts, decisionLiteral)
        // remove the negation of the literal from all clauses
        val updatedFormula =
          InternalCNF(SolverUtils.removeLiteralFromClauses(updatedClauses, decisionLiteral.negation))
        val newNode = DecisionLiteral(decisionLiteral.name, decisionLiteral.polarity, updatedFormula)
        lastNode.addChild(newNode)

        addToRootChoicesIfNecessary(graph, newNode, decisionLiteral)
        runCDCL(newNode, unsatWithNextConflict = false)
    }
  }

  private[this] def addToRootChoicesIfNecessary(graph: RootNode, lastNode: GraphNode, decisionLiteral: InternalLiteral): Boolean = {
    if (CDCLGraphUtils.getParentNodes(graph, lastNode).collect({case RootNode(_, _, _) => true}).nonEmpty) {
      if (rootChoices.contains(decisionLiteral)){
        // unsat
        return true
      } else {
        rootChoices += decisionLiteral
      }
    }
    false
  }

  /**
    * Pick the next decision literal.
    *
    * @return decision literal
    */
  private[this] def pickDecisionLiteral(formula: InternalCNF): InternalLiteral = {
    val possibleDecisionLiterals =
      formula.conjuncts.foldLeft[Set[InternalDisjunct]](Set())((s, d) => s ++ d.disjuncts.filter(d => d.isActive))
    if (possibleDecisionLiterals.isEmpty) {
      throw new IllegalStateException("Could not pick a decision literal! Formula: " + formula)
    } else {
      possibleDecisionLiterals.head.literal
    }
  }

}
