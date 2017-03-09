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
    val root = RootNode("root", _varValue = true, cnfRep)
    runCDCL(root, root) match {
      case None => None
      case Some(graph) => Some(graph.toModel)
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
  def runCDCL(graph: RootNode, lastNode: GraphNode): Option[RootNode] = {
    runToComplete(graph, lastNode)
    // TODO check for conflict and jump back in case of a conflict

    // TODO otherwise pick another decision literal and recurse!

    Some(graph)
  }

  /**
    * Returns true if the given model would have a conflict with the new variable assignment a
    */
  def wouldConflict(model: Map[String, Boolean], a: (String, Boolean)): Boolean = {
    if (model.contains(a._1)) {
      model(a._1) != a._2
    } else {
      false
    }
  }

  /**
    * Returns true if the given graph has a conflict
    */
  private[this] def hasConflict(graph: RootNode): Boolean = {
    ???
  }

  /**
    * Pick the next decision literal
    */
  def pickDecisionLiteral(formula: InternalCNF): InternalLiteral = {
    // for now just pick the first literal
    formula.conjuncts.head.disjuncts.head.literal
  }

  /**
    * Retruns a sequence of relevant decision literals in the order they appear in the graph
    */
  def relevantDecisionLiterals(graph: RootNode): Seq[GraphNode] = {
    ???
  }

  /**
    * Does the backjumping on the graph
    */
  def backJump(graph: RootNode, relevantLiterals: Seq[GraphNode]): Unit = {
    ???
  }

  /**
    * Returns the new claus that is introduced by the graph cut
    */
  def learnClause(graph: GraphNode): InternalClause = {
    // TODO cut the graph

    // TODO identify the edges that cross the boundary

    ???
  }

}

abstract class GraphNode(var varName: String, var varValue: Boolean, var formula: InternalCNF) {

  var children: Seq[GraphNode] = List()

  def addChild(newChild: GraphNode): Unit = {
    children = children.:+(newChild)
  }

  def addChildren(newChildren: Seq[GraphNode]): Unit = {
    children ++ newChildren
  }

  def toMap: Map[String, Boolean] = {
    this match {
      case RootNode(vN, vV, f) => Map()
      case NonDecisionLiteral(vN, vV, f) => Map(vN -> vV)
      case DecisionLiteral(vN, vV, f) => Map(vN -> vV)
    }
  }

  def toModel: Map[String, Boolean] = {
    if (children.isEmpty) {
      this.toMap
    } else {
      children
        .map(child => child.toModel)
        .reduceLeft((a, b) => a ++ b) ++ this.toMap
    }
  }

}

case class RootNode(_varName: String,
                    _varValue: Boolean,
                    _formula: InternalCNF)
  extends GraphNode(_varName, _varValue, _formula) {}

case class DecisionLiteral(_varName: String,
                           _varValue: Boolean,
                           _formula: InternalCNF)
  extends GraphNode(_varName, _varValue, _formula) {}

case class NonDecisionLiteral(_varName: String,
                              _varValue: Boolean,
                              _formula: InternalCNF)
  extends GraphNode(_varName, _varValue, _formula) {}
