package core

import smtlib.parser.Terms.Term
import util._

/**
  * Created by jan on 09.03.17.
  */
object CDCLSolver extends SATSolvingAlgorithm {

  /**
    * Applies the CDCL algorithm to the given formula. Returns None in case of unsat, otherwise the model mapping
    * variables to booleans.
    */
  override def checkSAT(formula: Term): Option[Map[String, Boolean]] = {
    val cnfRep: InternalCNF = CNFRepresentation.convertCNFToInternal(formula)
    val root = RootNode("root", varValue = true, cnfRep)
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
  def runToComplete(root: RootNode, lastNode: GraphNode): GraphNode = {
    if (lastNode.formula.conjuncts.isEmpty) {
      return lastNode
    }
    removeTautologies(lastNode.formula, root.toModel) match {
      case Some(f) =>
        lastNode.formula = f
        return runToComplete(root, lastNode)
      case None =>
    }
    applyUnitPropagation(lastNode.formula, root.toModel) match {
      case Some((f, r, unitClause)) =>
        // check for conflict and only continue with the recursion if there are none
        if (wouldConflict(root.toModel, r)) {
          lastNode.addChild(NonDecisionLiteral(r._1, r._2, f))
          return lastNode
        } else {
          val newNode = NonDecisionLiteral(r._1, r._2, f)
          lastNode.addChild(newNode)
          // find all the nodes that were previously in the literal, take their negation and add edges from them to the
          // new node in case they exists.
          val previouslyRemovedLiterals = unitClause.disjuncts.filter(d => !d.isActive)
          previouslyRemovedLiterals
            .map(l => l.literal)
            .map(l => CDCLGraphUtils.findNode(root, InternalLiteral(!l.polarity, l.name)))
            .collect({ case Some(n) => n })
            .foreach(n => n.addChild(newNode))
          return runToComplete(root, newNode)
        }
      case None =>
    }
    applyPureLiteralRule(lastNode.formula, root.toModel) match {
      case Some((f, r)) =>
        // check for conflict and only continue with the recursion if there are none
        if (wouldConflict(root.toModel, r)) {
          lastNode.addChild(NonDecisionLiteral(r._1, r._2, f))
          return lastNode
        } else {
          val newNode = NonDecisionLiteral(r._1, r._2, f)
          lastNode.addChild(newNode)
          return runToComplete(root, newNode)
        }
      case None =>
    }
    lastNode
  }

  /**
    * Recursively apply the CDCL steps until the SAT question is answered.
    */
  def runCDCL(graph: RootNode, lastNode: GraphNode): Boolean = {
    var newLastNode = runToComplete(graph, lastNode)
    val conflictVarName = CDCLGraphUtils.hasConflict(graph)
    conflictVarName match {
      case Some(name) =>
        val learnedClause = CDCLGraphUtils.learnClause(graph, name)

        if (learnedClause.disjuncts.size < 2) {
          // we have UNSAT
          return false
        }

        newLastNode = CDCLGraphUtils.doBackJumping(graph, name)

        CDCLGraphUtils.addClauseToAllFormulas(graph, learnedClause)
        runCDCL(graph, newLastNode)
      case None =>
        if (newLastNode.formula.conjuncts.isEmpty) {
          // if we processed the formula completely and there is no conflict, we are done and return SAT
          true
        } else {
          pickDecisionLiteral(newLastNode.formula) match {
            case Some(decisionLiteral) =>
              // remove all the clauses that contain the literal
              val updatedClauses = SolverUtils.takeClausesNotContainingLiteral(newLastNode.formula.conjuncts, decisionLiteral)
              // remove the negation of the literal from all clauses
              val updatedFormula = InternalCNF(SolverUtils.removeLiteralFromClauses(updatedClauses, decisionLiteral.negation))
              val newNode = DecisionLiteral(decisionLiteral.name, decisionLiteral.polarity, updatedFormula)
              newLastNode.addChild(newNode)
              runCDCL(graph, newNode)
            case None =>
              false
          }
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
  private[this] def pickDecisionLiteral(formula: InternalCNF): Option[InternalLiteral] = {
    val decisionLiterals = formula.conjuncts.head.disjuncts.filter(d => d.isActive)

    if (decisionLiterals.isEmpty) {
      return None
    }

    // for now just pick the first literal
    val decisionLiteral = decisionLiterals.head.literal

    Some(decisionLiteral)
  }

}
