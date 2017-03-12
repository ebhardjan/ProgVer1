package core

import smtlib.parser.Terms.Term
import util._

/**
  * Created by jan on 09.03.17.
  */
object CDCLSolver extends SATSolvingAlgorithm {

  var firstDecisionLiteral: InternalLiteral = _

  /**
    * Applies the CDCL algorithm to the given formula. Returns None in case of unsat, otherwise the model mapping
    * variables to booleans.
    */
  override def checkSAT(formula: Term): Option[Map[String, Boolean]] = {
    val cnfRep: InternalCNF = CNFRepresentation.convertCNFToInternal(formula)
    val root = RootNode("root", _varValue = true, cnfRep)
    if (runCDCL(root, root)) {
      Some(root.toModel)
    } else {
      None
    }
  }

  /**
    * Does tautology removal, unit propagation and pure literal elimination until either a conflict is reached, or we
    * cannot do anything anymore and need to guess another variable.
    *
    * @param root     the root node of the graph
    * @param lastNode the last added node, this is where the children will be added
    */
  def runToComplete(root: RootNode, lastNode: GraphNode): Unit = {
    if (lastNode.formula.conjuncts.isEmpty) {
      return
    }
    removeTautologies(lastNode.formula, root.toModel) match {
      case Some(f) =>
        lastNode.formula = f
        runToComplete(root, lastNode)
        return
      case None =>
    }
    applyUnitPropagation(lastNode.formula, root.toModel) match {
      // TODO construct the graph in a correct way!
      case Some((f, r)) =>
        // check for conflict and only continue with the recursion if there are none
        if (wouldConflict(root.toModel, r)) {
          lastNode.addChild(NonDecisionLiteral(r._1, r._2, f))
          return
        } else {
          val newNode = NonDecisionLiteral(r._1, r._2, f)
          lastNode.addChild(newNode)
          runToComplete(root, newNode)
          return
        }
      case None =>
    }
    applyPureLiteralRule(lastNode.formula, root.toModel) match {
      case Some((f, r)) =>
        // check for conflict and only continue with the recursion if there are none
        if (wouldConflict(root.toModel, r)) {
          lastNode.addChild(NonDecisionLiteral(r._1, r._2, f))
        } else {
          val newNode = NonDecisionLiteral(r._1, r._2, f)
          lastNode.addChild(newNode)
          runToComplete(root, newNode)
        }
      case None =>
    }
  }

  /**
    * Recursively apply the CDCL steps until the SAT question is answered.
    */
  def runCDCL(graph: RootNode, lastNode: GraphNode): Boolean = {
    runToComplete(graph, lastNode)
    val conflictVarName = CDCLGraphUtils.hasConflict(graph)
    conflictVarName match {
      case Some(name) =>
        val learnedClause = CDCLGraphUtils.learnClause(graph, name)
        val newLastNode = CDCLGraphUtils.doBackJumping(graph, name)

        if (newLastNode.varName.equals(firstDecisionLiteral.name) &&
          newLastNode.varValue.equals(!firstDecisionLiteral.polarity)){
          // we have UNSAT
          return false
        }

        CDCLGraphUtils.addClauseToAllFormulas(graph, learnedClause)
        runCDCL(graph, newLastNode)
      case None =>
        if (lastNode.formula.conjuncts.isEmpty) {
          // if we processed the formula completely and there is no conflict, we are done and return SAT
          true
        } else {
          val decisionLiteral = pickDecisionLiteral(lastNode.formula)
          // remove all the clauses that contain the literal
          val updatedClauses = SolverUtils.takeClausesNotContainingLiteral(lastNode.formula.conjuncts, decisionLiteral)
          // remove the negation of the literal from all clauses
          val updatedFormula = InternalCNF(SolverUtils.removeLiteralFromClauses(updatedClauses, decisionLiteral.negation))
          val newNode = DecisionLiteral(decisionLiteral.name, decisionLiteral.polarity, updatedFormula)
          lastNode.addChild(newNode)
          runCDCL(graph, lastNode)
        }
    }
  }

  /**
    * Returns true if the given model would have a conflict with the new variable assignment a
    */
  private[this] def wouldConflict(model: Map[String, Boolean], a: (String, Boolean)): Boolean = {
    if (model.contains(a._1)) {
      model(a._1) != a._2
    } else {
      false
    }
  }


  /**
    * Pick the next decision literal
    */
  private[this] def pickDecisionLiteral(formula: InternalCNF): InternalLiteral = {
    // for now just pick the first literal
    val decisionLiteral = formula.conjuncts.head.disjuncts.head.literal

    if (firstDecisionLiteral == null) {
      firstDecisionLiteral = decisionLiteral
    }

    decisionLiteral
  }

}
