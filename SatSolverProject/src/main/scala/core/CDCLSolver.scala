package core

import smtlib.parser.Terms.Term
import util._

/**
  * Created by jan on 09.03.17.
  */
class CDCLSolver extends SATSolvingAlgorithm {

  var rootChoices: Set[InternalLiteral] = Set()

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

    val root = RootNode("root", varValue = true, cnfRep)
    if (runCDCL(root, root)) {
      Some(root.toModel)
    } else {
      None
    }
  }

  def digestUnitPropagation(root: RootNode, f: InternalCNF, r: (String, Boolean), unitClause: InternalClause) = {
    val newNode = NonDecisionLiteral(r._1, r._2, f)
    // find all the nodes that were previously in the literal, take their negation and add edges from them to the
    // new node in case they exists.
    val previouslyRemovedLiterals = unitClause.disjuncts.filter(d => !d.isActive)
    if (previouslyRemovedLiterals.isEmpty) {
      root.addChild(newNode)
    } else {
      previouslyRemovedLiterals
        .map(l => l.literal)
        .map(l => CDCLGraphUtils.findNode(root, InternalLiteral(!l.polarity, l.name)))
        .collect({ case Some(n) => n })
        .foreach(n => n.addChild(newNode))
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
    if (CDCLGraphUtils.hasConflict(root).isDefined) {
      return
    }
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
    applyUnitPropagation(lastNode.formula) match {
      case Some((f, r, unitClause)) =>
        // add the negation to the graph in case we have empty disjuncts now!
        if (f.conjuncts.exists(c => !c.disjuncts.exists(d => d.isActive))) {
          // forcefully apply unit propagation on the negation of r to have the conflict in the graph!
          val negation = InternalClause(unitClause.clone().disjuncts
            .map(d => if (d.isActive) {
              InternalDisjunct(InternalLiteral(!d.literal.polarity, d.literal.name), isActive = true)
            } else {
              d
            }))
          val res = applyUnitPropagationOn(lastNode.formula, negation).get
          digestUnitPropagation(root, res._1, res._2, res._3)
        }
        digestUnitPropagation(root, f, r, unitClause)
        lastNode.formula = f
        runToComplete(root, lastNode)
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
  def runCDCL(graph: RootNode, lastNode: GraphNode): Boolean = {
    runToComplete(graph, lastNode)
    val conflictVarName = CDCLGraphUtils.hasConflict(graph)
    conflictVarName match {
      case Some(name) =>
        val learnedClause = CDCLGraphUtils.learnClause(graph, name)

        val newLastNode = CDCLGraphUtils.doBackJumping(graph, name)
        CDCLGraphUtils.addClauseToAllFormulas(graph, learnedClause)

        // TODO remove this duplicate code!
        val updatedClauses =
          SolverUtils.takeClausesNotContainingLiteral(newLastNode.formula.conjuncts,
            InternalLiteral(newLastNode.varValue, newLastNode.varName))
        // remove the negation of the literal from all clauses
        val updatedFormula =
          InternalCNF(SolverUtils.removeLiteralFromClauses(updatedClauses,
            InternalLiteral(!newLastNode.varValue, newLastNode.varName)))

        newLastNode.formula = updatedFormula
        if (addToRootChoicesIfNecessary(graph, newLastNode, InternalLiteral(newLastNode.varValue, newLastNode.varName))) {
         false
        } else {
          runCDCL(graph, newLastNode)
        }
      case None =>
        if (lastNode.formula.conjuncts.isEmpty) {
          // if we processed the formula completely and there is no conflict, we are done and return SAT
          return true
        }

        val decisionLiteral = pickDecisionLiteral(lastNode.formula)

        addToRootChoicesIfNecessary(graph, lastNode, decisionLiteral)

        val updatedClauses =
          SolverUtils.takeClausesNotContainingLiteral(lastNode.formula.conjuncts, decisionLiteral)
        // remove the negation of the literal from all clauses
        val updatedFormula =
          InternalCNF(SolverUtils.removeLiteralFromClauses(updatedClauses, decisionLiteral.negation))
        val newNode = DecisionLiteral(decisionLiteral.name, decisionLiteral.polarity, updatedFormula)
        lastNode.addChild(newNode)
        runCDCL(graph, newNode)
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
