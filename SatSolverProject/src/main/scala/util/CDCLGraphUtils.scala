package util

/**
  * Created by jan on 12.03.17.
  *
  * Util functions for the graph representation used in CDCL
  */
object CDCLGraphUtils {

  /**
    * Check if a given graph has a conflict
    *
    * @param graph the root node of the graph
    * @return Optional with String containing the name of the conflict variable or empty optional
    */
  def hasConflict(graph: RootNode): Option[String] = {
    var model: Map[String, Boolean] = Map()

    def _internalHasConflict(graph: GraphNode): Option[String] = {
      // Check the existing model. If there is a variable and it's negation in the graph we have a conflict.
      if (model.contains(graph.varName) && model(graph.varName) != graph.varValue) {
        // if we found the conflict return it
        Some(graph.varName)
      } else {
        // otherwise add the variable value of the current node to the graph and recurse
        model = model + (graph.varName -> graph.varValue)
        if (graph.children.isEmpty) {
          None
        } else {
          val conflicts = graph.children.map(c => _internalHasConflict(c)).collect({ case Some(s) => s })
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

    _internalHasConflict(graph)
  }

  /**
    * Returns a sequence of relevant decision literals in the order they appear in the graph
    *
    * @param graph           graph node from where to start the search
    * @param conflictVarName variable of which we want to find the relevant decision literals
    * @return list of relevant decision literals
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

  def decisionLiterals(graph: GraphNode): Seq[DecisionLiteral] = {
    graph match {
      case DecisionLiteral(_, _, _ ) => graph.children
        .foldLeft(Seq(graph.asInstanceOf[DecisionLiteral]))((s,c) => s ++: decisionLiterals(c))
      case RootNode(_, _, _) => graph.children
          .foldLeft[Seq[DecisionLiteral]](Seq())((s,c) => s ++: decisionLiterals(c))
      case _ => Seq()
    }
  }

  /**
    * helper function that returns whether a given decision literal can reach a conflict via only NonDecision literals
    */
  private[this] def decisionLiteralReachesConflict(node: DecisionLiteral, conflictVarName: String): Boolean = {
    /*
    def _reaches(node: GraphNode, allowDecisionLiteral: Boolean): Boolean = {
      if (conflictVarName.equals(node.varName)) {
        true
      } else {
        node.children
          .filter(c => c.isInstanceOf[NonDecisionLiteral] || allowDecisionLiteral)
          .foldLeft(false)((acc, c) => acc || _reaches(c, allowDecisionLiteral = false))
      }
    }
    */

    //_reaches(node, allowDecisionLiteral = true)
    node.children.exists(n => conflictVarName.equals(n.varName))
  }

  /**
    * Does the back-jumping on the graph as defined in the lecture slides.
    * Side effect: the graph is changed.
    *
    * @param graph              root node of the graph
    * @param conflictingVarName name of the variable that has a conflict
    * @return Newly added node. Where we want to proceed from in the next steps.
    */
  def doBackJumping(graph: RootNode, conflictingVarName: String): DecisionLiteral = {
    val relevantLiterals = relevantDecisionLiterals(graph, conflictingVarName)

    var rootOfDecisionLiteralRemoval: ADecisionLiteral = null
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

  /**
    * helper method that returns the first direct child that is a decision literal
    */
  private[this] def findFirstDecisionLiteralChild(node: GraphNode): DecisionLiteral = {
    val nextDecisionLiteralSet = node.children.filter(c => c.isInstanceOf[DecisionLiteral])
    if (nextDecisionLiteralSet.size > 1) {
      throw new IllegalStateException("Graph contains DecisionLiteral that has more than one DecisionLiteral as " +
        "children")
    } else if (nextDecisionLiteralSet.size == 1) {
      nextDecisionLiteralSet.head.asInstanceOf[DecisionLiteral]
    } else {
      null
    }
  }

  /**
    * deletes all direct and indirect children that are decision literals
    *
    * @param parentOfDecisionLiteral parent node of the first decision literal that will be deleted
    */
  private[this] def deleteAllDecisionLiteralsStartingWithChildOf(parentOfDecisionLiteral: GraphNode): Unit = {
    val nextDecisionLiteral = findFirstDecisionLiteralChild(parentOfDecisionLiteral)
    if (nextDecisionLiteral != null) {
      deleteAllDecisionLiteralsStartingWithChildOf(nextDecisionLiteral)
      parentOfDecisionLiteral.removeChild(nextDecisionLiteral)
    }
  }

  /**
    * deletes the literals that are part of a conflict in a given graph
    */
  private[this] def deleteConflictingLiteral(graph: GraphNode, conflictVarName: String): Unit = {
    graph.children.foreach(c =>
      if (c.varName.equals(conflictVarName)) {
        graph.removeChild(c)
      } else {
        deleteConflictingLiteral(c, conflictVarName)
      })
  }

  /**
    * deletes all non-decision literals in a graph that are not directly reachable by a decision literal
    */
  def deleteAllNotDirectlyReachableNonDecisionLiterals(graph: RootNode): Unit = {
    var reachable: Set[NonDecisionLiteral] = Set()

    def _traverseGraphAndAddToReachable(graph: GraphNode): Unit = {
      graph match {
        case g: NonDecisionLiteral => reachable += g
        case _ => graph.children.foreach(c => _traverseGraphAndAddToReachable(c))
      }
    }

    def _deleteAllElementsNotInReachable(graph: GraphNode): Unit = {
      graph.children.foreach {
        case child: NonDecisionLiteral =>
          if (!reachable.contains(child)) {
            graph.removeChild(child)
          }
          _deleteAllElementsNotInReachable(child)
        case child: Any => _deleteAllElementsNotInReachable(child)
      }
    }

    _traverseGraphAndAddToReachable(graph)
    _deleteAllElementsNotInReachable(graph)
  }


  /**
    * Returns the new claus that is introduced by the graph cut
    */
  def learnClause(graph: GraphNode, conflictVarName: String): InternalClause = {
    // Cut the graph such that conflicting literals are on one side and all decision literals on the other side

    // for now let's just do the most easy thing and just put the conflicts on one side and all the other nodes on the
    // other one.
    val conflict = NonDecisionLiteral(conflictVarName, varValue = true, null)
    val notConflict = NonDecisionLiteral(conflictVarName, varValue = false, null)
    val parents = getParentNodes(graph, conflict) ++ getParentNodes(graph, notConflict)

    // All the nodes that have outgoing edges are simply the parent nodes.
    // Create the disjunction of their negations
    val disjuncts = parents
      .filter(p => !p.isInstanceOf[RootNode])
      .map(p => InternalDisjunct(InternalLiteral(!p.varValue, p.varName), isActive = true))
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
        val res = graph.children.map(c => findNode(c, needle)).collect({ case Some(n) => n })
        if (res.size > 1) {
          throw new IllegalStateException("Graph contains the same element multiple times!")
        } else if (res.size == 1) {
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
  def addClauseToAllFormulas(graph: ADecisionLiteral, learnedClause: InternalClause): Unit = {
    graph.formula = InternalCNF(graph.formula.conjuncts + learnedClause)
    if (graph.children.nonEmpty) {
      graph.children.foreach {
        case c@DecisionLiteral(_, _, _) => addClauseToAllFormulas(c.asInstanceOf[DecisionLiteral], learnedClause)
      }
    }
  }

}
