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
  def runToComplete(root: RootNode, lastNode: GraphNode): GraphNode = {
    if (CDCLGraphUtils.hasConflict(root).isDefined) {
      return lastNode
    }
    if (lastNode.formula.conjuncts.isEmpty) {
      return lastNode
    }
    removeTautologies(lastNode.formula, root.toModel) match {
      case Some(f) =>
        lastNode.formula = f
        return runToComplete(root, lastNode)
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
        return runToComplete(root, lastNode)
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

        if (learnedClause.disjuncts.size < 1) {
          // we have UNSAT
          return false
        }

        newLastNode = CDCLGraphUtils.doBackJumping(graph, name)
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
        if (newLastNode.formula.conjuncts.isEmpty) {
          // if we processed the formula completely and there is no conflict, we are done and return SAT
          true
        } else {
          pickDecisionLiteral(newLastNode.formula) match {
            case Some(decisionLiteral) =>
              if (addToRootChoicesIfNecessary(graph, newLastNode, decisionLiteral)) {
                // unsat
                return false
              }
              // remove all the clauses that contain the literal
              val updatedClauses =
                SolverUtils.takeClausesNotContainingLiteral(newLastNode.formula.conjuncts, decisionLiteral)
              // remove the negation of the literal from all clauses
              val updatedFormula =
                InternalCNF(SolverUtils.removeLiteralFromClauses(updatedClauses, decisionLiteral.negation))
              val newNode = DecisionLiteral(decisionLiteral.name, decisionLiteral.polarity, updatedFormula)
              newLastNode.addChild(newNode)
              runCDCL(graph, newNode)
            case None =>
              // no more literals left to pick from -> UNSAT
              false
          }
        }
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
    * Pick the next decision literal.
    *
    * @return optional containing a decision literal or empty optional in case we cannot pick a new decision literal
    *         because there are no more literals to choose from left
    */
  private[this] def pickDecisionLiteral(formula: InternalCNF): Option[InternalLiteral] = {
    val possibleDecisionLiterals =
      formula.conjuncts.foldLeft[Set[InternalDisjunct]](Set())((s, d) => s ++ d.disjuncts.filter(d => d.isActive))

    if (possibleDecisionLiterals.isEmpty) {
      None
    } else {
      Some(possibleDecisionLiterals.head.literal)
    }

  }

}
