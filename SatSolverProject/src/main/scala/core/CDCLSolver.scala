package core

import smtlib.parser.Terms.Term
import util._

/**
  * Created by jan on 09.03.17.
  */


object CDCLSolverWrapper extends SATSolvingAlgorithm {

  override def checkSAT(formula: Term): Option[Map[String, Boolean]] = {
    (new CDCLSolver).checkSAT(formula)
  }
}

class CDCLSolver extends SATSolvingAlgorithm {

  // For debugging purposes, needs to be deactivated for performance tests.
  // The file access massively slows down the solver.
  val writeDotGraph: Boolean = false

  // Flag to turn on and off the usage of the pure literal rule
  val usePureLiteralRule: Boolean = true

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
    if (runCDCL(graph)) {
      Some(SolverUtils.setAllRemainingVariables(cnfRep, graph.toModel))
    } else {
      None
    }
  }

  /**
    * given a decision literal, new assignment and unit clause used for the unit propagation, find all the nodes that
    * were previously in the unit clause and add edges for them in the graph.
    */
  def digestUnitPropagation(lastNode: ADecisionLiteral, r: (String, Boolean), unitClauses: Set[InternalClause]): Unit = {
    val newNode = NonDecisionLiteral(r._1, r._2, lastNode)

    // add the node to the implications of the last decision literal
    lastNode.addDecisionImplication(newNode)

    // find all the nodes that were previously in the clause, take their negation and add edges from them to the new
    // node in case they exist.
    val previouslyRemovedLiterals = unitClauses.foldLeft[Set[InternalLiteral]](Set())(
      (acc, u) => acc ++ u.disjuncts.filter(d => !d.isActive).map(d => d.literal))
    if (previouslyRemovedLiterals.isEmpty) {
      graph.addChild(newNode)
    } else {
      previouslyRemovedLiterals
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
      case Some((f, r, unitClauses)) =>
        // TODO: this code block is rather large...
        var formula = f
        // in case we have empty disjuncts now, also apply unit propagation on the negation of r
        if (f.conjuncts.exists(c => !c.disjuncts.exists(d => d.isActive))) {
          // take the first clause where the negation of r appears, set the negation to active and propagate
          val emptyClauses = f.conjuncts
            .filter(c => !c.disjuncts.exists(d => d.isActive))
            .filter(c => c.disjuncts.exists(d => d.literal.name.equals(r._1) && d.literal.polarity.equals(!r._2)))
          val emptyClause = emptyClauses.head
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
        digestUnitPropagation(lastNode, r, unitClauses)
        lastNode.formula = formula
        runToComplete(lastNode)
      case None =>
    }
    if (usePureLiteralRule) {
      applyPureLiteralRule(lastNode.formula, graph.toModel) match {
        case Some((f, r)) =>
          val newNode = NonDecisionLiteral(r._1, r._2, lastNode)
          lastNode.addChild(newNode)
          lastNode.formula = f
          runToComplete(lastNode)
        case None =>
      }
    }
  }

  /**
    * Recursively apply the CDCL steps until the SAT question is answered.
    */
  def runCDCL(lastNode: ADecisionLiteral): Boolean = {
    if (writeDotGraph) {
      GraphVisualizer.writeXdot(graph)
    }

    runToComplete(lastNode)
    val conflictVarName = CDCLGraphUtils.hasConflict(graph)

    if (writeDotGraph) {
      GraphVisualizer.writeXdot(graph)
    }

    conflictVarName match {
      case Some(name) =>
        // TODO: split up this code block and put it in separate methods. Maybe even an object "ConflictResolver"?
        // conflict without even making a choice!
        if (!graph.children.exists({ case _: DecisionLiteral => true case _ => false })) {
          return false
        }

        val learnedClause = CDCLGraphUtils.learnClause(graph, name)

        var (newLastNodeParent, newVarName, newVarValue) = CDCLGraphUtils.doBackJumping(graph, name)

        // if we already backjumped to this node, lets backtrack one step further
        if (newLastNodeParent.triedChildNodes.keys
          .exists(l => l.name.equals(newVarName) && l.polarity.equals(newVarValue))) {
          if (!newLastNodeParent.isInstanceOf[RootNode]) {
            newVarName = newLastNodeParent.varName
            newVarValue = !newLastNodeParent.varValue
            newLastNodeParent = CDCLGraphUtils.getParentNodes(graph, newLastNodeParent)
              .filter(n => n.isInstanceOf[DecisionLiteral] || n.isInstanceOf[RootNode])
              .map(n => n.asInstanceOf[ADecisionLiteral])
              .head
            CDCLGraphUtils.deleteAllDecisionLiteralsStartingWithChildOf(graph, newLastNodeParent)
          }
        }
        // add the newLastNode as a "tried child" to the newLastNodeParent only if we didn't learn anything new
        // (*) note: we added this at some stage to avoid infinite recursion. (The tests stop earlier and don't run into
        // a stack-overflow but still fail if the returned result is wrong). In the final version solver we would
        // probably not need it, at least we didn't find any cases where the if branch is executed.
        if (newLastNodeParent.formula.conjuncts.contains(learnedClause)) {
          newLastNodeParent.addTriedChild(newVarName, newVarValue)
        }
        CDCLGraphUtils.addClauseToAllFormulas(graph, learnedClause)

        if (writeDotGraph) {
          GraphVisualizer.writeXdot(graph)
        }

        // just for the new clause that we just learned
        runToComplete(newLastNodeParent)

        // only add the decision literal if it is not already implied
        val newLastNode = {
          if (CDCLGraphUtils.findNode(graph, InternalLiteral(newVarValue, newVarName)).isEmpty &&
            CDCLGraphUtils.findNode(graph, InternalLiteral(!newVarValue, newVarName)).isEmpty &&
            CDCLGraphUtils.hasConflict(graph).isEmpty) {
            val dl = DecisionLiteral(newVarName, newVarValue, newLastNodeParent.formula)
            val updatedClauses =
              SolverUtils.takeClausesNotContainingLiteral(dl.formula.conjuncts,
                InternalLiteral(dl.varValue, dl.varName))
            // remove the negation of the literal from all clauses
            val updatedFormula =
              InternalCNF(SolverUtils.removeLiteralFromClauses(updatedClauses,
                InternalLiteral(!dl.varValue, dl.varName)))

            dl.formula = updatedFormula
            newLastNodeParent.addChild(dl)
            // note: see (*) comment from above
            if (newLastNodeParent.abort()) {
              return false
            }
            dl
          } else {
            newLastNodeParent
          }
        }

        runCDCL(newLastNode)
      case None =>
        // if we processed the formula completely and there is no conflict, we are done and return SAT
        if (lastNode.formula.conjuncts.isEmpty) {
          return true
        }

        val decisionLiteral = pickDecisionLiteral(lastNode.formula, graph.toModel)

        val updatedClauses =
          SolverUtils.takeClausesNotContainingLiteral(lastNode.formula.conjuncts, decisionLiteral)
        // remove the negation of the literal from all clauses
        val updatedFormula =
          InternalCNF(SolverUtils.removeLiteralFromClauses(updatedClauses, decisionLiteral.negation))
        val newNode = DecisionLiteral(decisionLiteral.name, decisionLiteral.polarity, updatedFormula)
        lastNode.addChild(newNode)

        runCDCL(newNode)
    }
  }

  /**
    * Pick the next decision literal.
    *
    * @return decision literal
    */
  private def pickDecisionLiteral(formula: InternalCNF, model: Map[String, Boolean]): InternalLiteral = {
    val possibleDecisionLiterals =
      formula.conjuncts.foldLeft[Set[InternalDisjunct]](Set())(
        (s, d) => s ++ d.disjuncts.filter(d => d.isActive).filter(d => !model.contains(d.literal.name)))
    if (possibleDecisionLiterals.isEmpty) {
      throw new IllegalStateException("Could not pick a decision literal! Formula: " + formula)
    } else {
      possibleDecisionLiterals.head.literal
    }
  }

}
