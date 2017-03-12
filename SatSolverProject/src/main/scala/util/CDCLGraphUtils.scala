package util

/**
  * Created by jan on 12.03.17.
  *
  * Util functions for the graph representation used in CDCL
  */
object CDCLGraphUtils {

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
  def learnClause(graph: GraphNode, conflictVarName: String): InternalClause = {
    // Cut the graph such that conflicting literals are on one side and all decision literals on the other side

    // for now let's just do the most easy thing and just put the conflicts on one side and all the other nodes on the
    // other one.
    val conflict = NonDecisionLiteral(conflictVarName, _varValue = true, null)
    val notConflict = NonDecisionLiteral(conflictVarName, _varValue = false, null)
    val parents = getParentNodes(graph, conflict) ++ getParentNodes(graph, notConflict)

    // All the nodes that have outgoing edges are simply the parent nodes.
    // Create the disjunction of their negations
    val disjuncts = parents.map(p => InternalDisjunct(InternalLiteral(!p.varValue, p.varName), isActive = true))
    InternalClause(disjuncts)
  }

  /**
    * Returns the set of parent nodes given any node in the graph.
    */
  def getParentNodes(graph: GraphNode, needle: GraphNode): Set[GraphNode] = {
    if (graph.children.isEmpty) {
      Set()
    } else {
      graph.children.foldLeft[Set[GraphNode]](Set())((s, c) => {
        if (c.equals(needle)) {
          s + graph ++ getParentNodes(c, needle)
        } else {
          s ++ getParentNodes(c, needle)
        }
      })
    }
  }

  /**
    * Returns a node for the literal in the graph
    */
  def findNode(graph: GraphNode, needle: InternalLiteral): Option[GraphNode] = {
    if (graph.varName.equals(needle.name) && graph.varValue.equals(needle.polarity)) {
      Some(graph)
    } else {
      if (graph.children.nonEmpty) {
        val res = graph.children.map(c => findNode(c, needle)).collect({case Some(n) => n})
        if (res.size > 1) {
          throw new IllegalStateException("Graph contains the same element multiple times!")
        } else if(res.size == 1) {
          Some(res.iterator.next)
        } else {
          None
        }
      } else {
        None
      }
    }
  }

  /**
    * Adds the given class to all formulas in the given graph
    *
    * @param learnedClause the new clause we want to just learned and want to add to all formulas
    */
  def addClauseToAllFormulas(graph: GraphNode, learnedClause: InternalClause): Unit = {
    graph.formula = InternalCNF(graph.formula.conjuncts + learnedClause)
    if (graph.children.nonEmpty) {
      graph.children.foreach(c => addClauseToAllFormulas(c, learnedClause))
    }
  }

}
