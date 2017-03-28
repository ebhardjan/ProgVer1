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
    graph.allNodes foreach { case (_, v) =>
      if (model.contains(v.varName) && model(v.varName) != v.varValue) {
        return Some(v.varName)
      } else {
        model = model + (v.varName -> v.varValue)
      }
    }
    None
  }

  /**
    * Returns a sequence of relevant decision literals in the order they appear in the graph
    *
    * @param graph           graph node from where to start the search
    * @param conflictVarName variable of which we want to find the relevant decision literals
    * @return list of relevant decision literals
    */
  // TODO optimize here
  def relevantDecisionLiterals(graph: RootNode, conflictVarName: String): Seq[DecisionLiteral] = {
    def _relevantDecisionLiterals(graph: GraphNode): Seq[DecisionLiteral] = {
      graph match {
        case d: DecisionLiteral =>
          if (decisionLiteralReachesConflict(d, conflictVarName)) {
            d.children.foldLeft(Seq(d))((s, c) => s ++: _relevantDecisionLiterals(c))
          } else {
            d.children.foldLeft[Seq[DecisionLiteral]](Seq())((s, c) => s ++: _relevantDecisionLiterals(c))
          }
        case r: RootNode =>
          r.children.foldLeft[Seq[DecisionLiteral]](Seq())((s, c) => s ++: _relevantDecisionLiterals(c))
        case _ => Seq()
      }
    }

    _relevantDecisionLiterals(graph)
  }

  def decisionLiterals(graph: GraphNode): Seq[DecisionLiteral] = {
    graph match {
      case DecisionLiteral(_, _, _) => graph.children
        .foldLeft(Seq(graph.asInstanceOf[DecisionLiteral]))((s, c) => s ++: decisionLiterals(c))
      case RootNode(_, _, _) => graph.children
        .foldLeft[Seq[DecisionLiteral]](Seq())((s, c) => s ++: decisionLiterals(c))
      case _ => Seq()
    }
  }

  /**
    * helper function that returns whether a given decision literal can reach a conflict via only NonDecision literals
    */
  private[this] def decisionLiteralReachesConflict(node: DecisionLiteral, conflictVarName: String): Boolean = {
    def _reaches(graphNode: GraphNode): Boolean = {
      if (graphNode.varName.equals(conflictVarName)) {
        true
      } else {
        graphNode.children.collect({ case d: NonDecisionLiteral => d }).foldLeft(false)((acc, n) => acc || _reaches(n))
      }
    }

    node.decisionImplies.exists(n => conflictVarName.equals(n.varName)) ||
      _reaches(node)
  }

  /**
    * Does the back-jumping on the graph as defined in the lecture slides.
    * Side effect: the graph is changed.
    *
    * @param graph              root node of the graph
    * @param conflictingVarName name of the variable that has a conflict
    * @return Newly added node. Where we want to proceed from in the next steps.
    */
  def doBackJumping(graph: RootNode, conflictingVarName: String): (ADecisionLiteral, String, Boolean) = {
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
    deleteAllDecisionLiteralsStartingWithChildOf(graph, rootOfDecisionLiteralRemoval)

    // return the negation of the last relevant decision literal
    val removedRelevantDecisionLiteral = relevantLiterals.last
    (rootOfDecisionLiteralRemoval, removedRelevantDecisionLiteral.varName, !removedRelevantDecisionLiteral.varValue)
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
    * deletes all direct and indirect children that are decision literals and the non decision literals
    * that got introduced because of those decision literals
    *
    * @param root                    the root node of the graph
    * @param parentOfDecisionLiteral parent node of the first decision literal that will be deleted
    */
  def deleteAllDecisionLiteralsStartingWithChildOf(root: RootNode, parentOfDecisionLiteral: GraphNode): Unit = {
    val nextDecisionLiteral = findFirstDecisionLiteralChild(parentOfDecisionLiteral)
    if (nextDecisionLiteral != null) {
      deleteAllDecisionLiteralsStartingWithChildOf(root, nextDecisionLiteral)
      // delete all the non decision literals that got implied because of this decision literal
      nextDecisionLiteral.decisionImplies.foreach(nd => {
        root.removeNode(nd)
        deleteNonDecisionLiteralFromGraph(root, nd.varName, nd.varValue)
      })
      root.removeNode(nextDecisionLiteral)
      parentOfDecisionLiteral.removeChild(nextDecisionLiteral)
    }
  }

  /**
    * Deletes a non-decision literal from the graph
    */
  private def deleteNonDecisionLiteralFromGraph(graph: RootNode, varName: String, varValue: Boolean): Unit = {
    graph.allNodes foreach { case (_, v) =>
      v.children.foreach(c =>
        if (c.varName.equals(varName) && c.varValue.equals(varValue)) {
          v.removeChild(c)
        }
      )
    }
  }

  /**
    * Returns the new claus that is introduced by the graph cut
    */
  def learnClause(graph: RootNode, conflictVarName: String): InternalClause = {
    // Cut the graph such that conflicting literals are on one side and all decision literals on the other side

    // for now let's just do the most easy thing and just put the conflicts on one side and all the other nodes on the
    // other one.
    val conflict = NonDecisionLiteral(conflictVarName, varValue = true, null)
    val notConflict = NonDecisionLiteral(conflictVarName, varValue = false, null)
    val parents = getParentNodes(graph, conflict) ++ getParentNodes(graph, notConflict)

    // Create the disjunction of their negations
    val disjuncts = parents
      .filter(p => !p.isInstanceOf[RootNode])
      .map(p => InternalDisjunct(InternalLiteral(!p.varValue, p.varName), isActive = true))
    InternalClause(disjuncts)
  }

  /**
    * Returns the set of parent nodes given any node in the graph.
    */
  def getParentNodes(graph: RootNode, needle: GraphNode): Set[GraphNode] = {
    var parents: Set[GraphNode] = Set()
    graph.allNodes foreach { case (_, v) =>
      if (v.children.exists(c => c.equals(needle))) {
        parents += v
      }
    }
    parents
  }

  /**
    * Returns a node for the literal in the graph
    */
  def findNode(graph: RootNode, needle: InternalLiteral): Option[GraphNode] = {
    graph.allNodes.get(needle.toString)
  }

  /**
    * Adds the given class to all formulas in the given graph
    *
    * @param learnedClause the new clause we want to just learned and want to add to all formulas
    */
  def addClauseToAllFormulas(root: RootNode, learnedClause: InternalClause): Unit = {
    // TODO: loosing one second here!
      root.formula = {
        deactivateAlreadyChosenLiterals(learnedClause, getPartialModel(root, root)) match {
          case Some(f) => InternalCNF(root.formula.conjuncts + f)
          case None => root.formula
        }
      }
      root.allNodes foreach { case (_, v) =>
        v match {
          case graph: DecisionLiteral =>
            val partialModel = getPartialModel(root, graph)
            graph.formula = {
              deactivateAlreadyChosenLiterals(learnedClause, partialModel) match {
                case Some(f) => InternalCNF(graph.formula.conjuncts + f)
                case None => graph.formula
              }
            }
          case _ =>
        }
      }
  }

  /**
    * Deactivates the disjuncts in a clause whose negation is in a given model or partial model.
    * If the clause contains a literal that is contained in the model, None is returned.
    */
  private def deactivateAlreadyChosenLiterals(clause: InternalClause, partialModel: Map[String, Boolean])
  : Option[InternalClause] = {
    if (clause.disjuncts.exists(d => partialModel.contains(d.literal.name)
      && partialModel(d.literal.name) == d.literal.polarity)) {
      None
    } else {
      val disjuncts = clause.disjuncts.map(d => {
        if (partialModel.contains(d.literal.name) && partialModel(d.literal.name) == !d.literal.polarity) {
          InternalDisjunct(InternalLiteral(d.literal.polarity, d.literal.name), isActive = false)
        } else {
          d
        }
      })
      Some(InternalClause(disjuncts))
    }
  }

  /**
    * traverses the tree starting from the top and collects the parts of the model that we already decided on or are
    * already given before we made the decision specified by targetNode
    *
    * @param currentRoot root node of the graph
    * @param targetNode  DecisionLiteral up to whom we want to get the model
    * @return partial model
    */
  private def getPartialModel(currentRoot: ADecisionLiteral, targetNode: ADecisionLiteral): Map[String, Boolean] = {
    def _getDecisionImplies(): Map[String, Boolean] = {
      currentRoot.decisionImplies.map(nd => nd.varName -> nd.varValue).toMap
    }

    if (currentRoot.equals(targetNode)) {
      currentRoot match {
        case _: RootNode => _getDecisionImplies()
        case _ => Map(currentRoot.varName -> currentRoot.varValue) ++ _getDecisionImplies()
      }
    } else {
      val decisionImplies: Map[String, Boolean] = _getDecisionImplies()
      currentRoot.children
        .filter(n => n.isInstanceOf[DecisionLiteral])
        .map(n => n.asInstanceOf[DecisionLiteral])
        .foldLeft(decisionImplies)((acc, c) => acc + (c.varName -> c.varValue) ++ getPartialModel(c, targetNode))
    }
  }

}
