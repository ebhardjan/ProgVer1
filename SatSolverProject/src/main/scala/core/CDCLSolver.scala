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

  private[this] def addClauseToAllFormulas(graph: GraphNode, learnedClause: InternalClause): Unit = {
    graph.formula = InternalCNF(graph.formula.conjuncts + learnedClause)
    if (graph.children.nonEmpty) {
      graph.children.foreach(c => addClauseToAllFormulas(c, learnedClause))
    }
  }

  /**
    * Recursively apply the CDCL steps until the SAT question is answered.
    */
  def runCDCL(graph: RootNode, lastNode: GraphNode): Boolean = {
    // TODO currently the unsat case is not detected!
    runToComplete(graph, lastNode)
    val conflictVarName = hasConflict(graph)
    conflictVarName match {
      case Some(name) =>
        val learnedClause = learnClause(graph)
        val newLastNode = doBackJumping(graph, name)
        addClauseToAllFormulas(graph, learnedClause)
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
  def hasConflict(graph: RootNode): Option[String] = {
    var model: Map[String, Boolean] = Map()

    def internalHasConflict(graph: GraphNode): Option[String] = {
      if (model.contains(graph.varName) && model(graph.varName) != graph.varValue) {
        Some(graph.varName)
      } else {
        model = model + (graph.varName -> graph.varValue)
        if (graph.children.isEmpty) {
          None
        } else {
          val conflicts = graph.children.map(c => internalHasConflict(c)).collect({ case Some(s) => s })
          if (conflicts.size == 1) {
            Some(conflicts.head)
          } else if (conflicts.size > 1) {
            throw new IllegalStateException("More than one conflict in graph!")
          } else {
            None
          }
        }
      }
    }

    internalHasConflict(graph)
  }

  /**
    * Pick the next decision literal
    */
  private[this] def pickDecisionLiteral(formula: InternalCNF): InternalLiteral = {
    // for now just pick the first literal
    formula.conjuncts.head.disjuncts.head.literal
  }

  /**
    * Returns a sequence of relevant decision literals in the order they appear in the graph
    */
  def relevantDecisionLiterals(graph: GraphNode, conflictVarName: String): Seq[DecisionLiteral] = {
    graph match {
      case DecisionLiteral(_, _, _) =>
        if (decisionLiteralReachesConflict(graph.asInstanceOf[DecisionLiteral], conflictVarName)) {
          graph.children
            .foldLeft(Seq(graph.asInstanceOf[DecisionLiteral]))((s, c) => s ++: relevantDecisionLiterals(c, conflictVarName))
        } else {
          graph.children
            .foldLeft[Seq[DecisionLiteral]](Seq())((s, c) => s ++: relevantDecisionLiterals(c, conflictVarName))
        }
      case RootNode(_, _, _) => graph.children
        .foldLeft[Seq[DecisionLiteral]](Seq())((s, c) => s ++: relevantDecisionLiterals(c, conflictVarName))
      case _ => Seq()
    }
  }

  private[this] def decisionLiteralReachesConflict(node: DecisionLiteral, conflictVarName: String): Boolean = {
    def reaches(node: GraphNode): Boolean = {
      node.children.foldLeft(false)(
        (b, c) => c match {
          case NonDecisionLiteral(varName, _, _) =>
            if (conflictVarName.equals(varName)) {
              return true
            } else {
              return b || reaches(c)
            }
          case _ => false
        }
      )
    }

    reaches(node)
  }

  /**
    * Does the back-jumping on the graph
    */
  def doBackJumping(graph: RootNode, conflictingVarName: String): GraphNode = {
    val relevantLiterals = relevantDecisionLiterals(graph, conflictingVarName)

    var rootOfDecisionLiteralRemoval: GraphNode = null
    if (relevantLiterals.length > 1) {
      // case where we remove all decision literals starting from the next child of the second last relevant one
      rootOfDecisionLiteralRemoval = relevantLiterals(relevantLiterals.length - 2)
    } else {
      // case where we remove all decision literals, because there is only one relevant one
      rootOfDecisionLiteralRemoval = graph
    }

    // cut only one relevant literal + as many others as possible
    deleteAllDecisionLiteralsStartingWithChildOf(rootOfDecisionLiteralRemoval)
    // remove the conflict nodes from the graph
    deleteConflictingLiteral(graph, conflictingVarName)
    // remove any other not directly reachable NonDecisionLiteral nodes from the graph
    deleteAllNotDirectlyReachableNonDecisionLiterals(graph)

    // add the negation of the last relevant decision literal
    val removedRelevantDecisionLiteral = relevantLiterals.last
    val newDecisionLiteral = DecisionLiteral(removedRelevantDecisionLiteral.varName,
      !removedRelevantDecisionLiteral.varValue,
      rootOfDecisionLiteralRemoval.formula)
    rootOfDecisionLiteralRemoval.addChild(newDecisionLiteral)

    newDecisionLiteral
  }

  private[this] def getParentNode(graph: GraphNode, needle: GraphNode): Option[GraphNode] = {
    if (graph.children.contains(needle)) {
      Some(graph)
    } else {
      val res = graph.children.map(c => getParentNode(c, needle)).collect({ case Some(graphNode) => graphNode })
      if (res.size == 1) {
        Some(res.toIterator.next())
      } else {
        None
      }
    }
  }

  private[this] def findFirstDecisionLiteralChild(graph: GraphNode): DecisionLiteral = {
    val nextDecisionLiteralSet = graph.children.filter(c => c.isInstanceOf[DecisionLiteral])
    if (nextDecisionLiteralSet.size > 1) {
      throw new IllegalStateException("Graph contains DecisionLiteral that has more than one DecisionLiteral as " +
        "children")
    } else if (nextDecisionLiteralSet.size == 1) {
      nextDecisionLiteralSet.head.asInstanceOf[DecisionLiteral]
    } else {
      null
    }
  }

  private[this] def deleteAllDecisionLiteralsStartingWithChildOf(parentOfDecisionLiteral: GraphNode): Unit = {
    val nextDecisionLiteral = findFirstDecisionLiteralChild(parentOfDecisionLiteral)
    if (nextDecisionLiteral != null) {
      deleteAllDecisionLiteralsStartingWithChildOf(nextDecisionLiteral)
      parentOfDecisionLiteral.removeChild(nextDecisionLiteral)
    }
  }

  private[this] def deleteConflictingLiteral(graph: GraphNode, conflictVarName: String): Unit = {
    graph.children.foreach(c =>
      if (c.varName.equals(conflictVarName)) {
        graph.removeChild(c)
      } else {
        deleteConflictingLiteral(c, conflictVarName)
      })
  }

  def deleteAllNotDirectlyReachableNonDecisionLiterals(graph: GraphNode): Unit = {
    var reachable: Set[NonDecisionLiteral] = Set()

    def traverseGraphAndAddToReachable(graph: GraphNode): Unit = {
      graph match {
        case g: NonDecisionLiteral => reachable += g
        case _ => graph.children.foreach(c => traverseGraphAndAddToReachable(c))
      }
    }

    def deleteAllElementsNotInReachable(graph: GraphNode): Unit = {
      graph.children.foreach {
        case child: NonDecisionLiteral =>
          if (!reachable.contains(child)) {
            graph.removeChild(child)
          }
          deleteAllElementsNotInReachable(child)
        case child: Any => deleteAllElementsNotInReachable(child)
      }
    }

    traverseGraphAndAddToReachable(graph)
    deleteAllElementsNotInReachable(graph)
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

  var children: Set[GraphNode] = Set()

  def addChild(newChild: GraphNode): Unit = {
    val beforeCount = children.size
    children += newChild
    val afterCount = children.size
    if (afterCount != beforeCount + 1) {
      throw new IllegalStateException("Set of children of graph node did not grow when adding one element! " +
        "Does the graph already contain the added child?")
    }
  }

  def removeChild(remove: GraphNode): Unit = {
    val beforeCount = children.size
    children -= remove
    val afterCount = children.size
    if (afterCount != beforeCount - 1) {
      throw new IllegalStateException("Set of children of graph node did not shrink when removing one element! " +
        "Does the graph not contain the child?")
    }
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
