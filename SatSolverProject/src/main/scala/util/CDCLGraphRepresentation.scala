package util

import scala.collection.mutable

/**
  * Created by jan on 12.03.17.
  *
  * Graph representation used in the CDCL algorithm.
  */

/**
  * Abstract base class for any graph node.
  *
  * Implements some important functionality such as adding and removing children and converting to a model.
  */
abstract class GraphNode() {

  var varName: String
  var varValue: Boolean

  var children: Set[GraphNode] = Set()

  /**
    * adds the given child to the graph
    *
    * @param newChild the child to add
    */
  def addChild(newChild: GraphNode): Unit = {
    children += newChild
  }

  /**
    * removes the given child from the graph
    *
    * @param remove the child to be removed
    */
  def removeChild(remove: GraphNode): Unit = {
    children = children.filter(n => !n.equals(remove)) //TODO: why on earth does children -= remove not work?
  }

  /**
    * Converts the graph into a model.
    *
    * @return map of variable names to variable values (true or false)
    */
  def toModel: Map[String, Boolean] = {
    if (children.isEmpty) {
      this.toMap
    } else {
      children
        .map(child => child.toModel)
        .reduceLeft((a, b) => a ++ b) ++ this.toMap
    }
  }

  private[this] def toMap: Map[String, Boolean] = {
    this match {
      case RootNode(_, _, _) => Map()
      case NonDecisionLiteral(vN, vV, _) => Map(vN -> vV)
      case DecisionLiteral(vN, vV, _) => Map(vN -> vV)
    }
  }

  override def toString: String = {
    var res: String = ""
    if (!varValue) {
      res += "!"
    }
    res + varName
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case graphNode: GraphNode =>
        graphNode.varName.equals(this.varName) && graphNode.varValue.equals(this.varValue)
      case _ => false
    }
  }

  /**
    * equals method not just on the graph node but on the whole graph (all children are equals as well)
    *
    * useful for testing...
    */
  def recursiveEquals(other: GraphNode): Boolean = {
    val currentNode = other.varName.equals(this.varName) && other.varValue.equals(this.varValue)
    val childCount = other.children.size == children.size
    if (children.nonEmpty) {
      val thisChildren = children.toSeq.sortWith(_.varName < _.varName)
      val otherChildren = other.children.toSeq.sortWith(_.varName < _.varName)
      val zipped = thisChildren.zip(otherChildren)
      zipped.foldLeft(currentNode && childCount)((acc, z) => acc && z._1.recursiveEquals(z._2))
    } else {
      currentNode && childCount
    }
  }
}

abstract class ADecisionLiteral() extends GraphNode() {
  var varName: String
  var varValue: Boolean
  var formula: InternalCNF

  // the NonDecisionLiterals that have been added because of this decision literal
  var decisionImplies: Set[NonDecisionLiteral] = Set()

  // children that have been tried as DecisionLiterals
  var triedChildNodes: Map[InternalLiteral, Integer] = Map()

  def addTriedChild(varName: String, varValue: Boolean): Unit = {
    val literal = InternalLiteral(varValue, varName)
    if (triedChildNodes.contains(literal)) {
      triedChildNodes += (literal -> (triedChildNodes(literal) + 1))
    } else {
      triedChildNodes += (literal -> 1)
    }
  }

  def abort(): Boolean = {
    triedChildNodes.values.exists(i => i > 2)
  }

  /**
    * Add a new NonDecision literal that go introduced because of this decision literal (or the root)
    *
    * @param implication the non decision literal to add
    */
  def addDecisionImplication(implication: NonDecisionLiteral) {
    decisionImplies = decisionImplies + implication
  }
}

case class RootNode(override var varName: String,
                    override var varValue: Boolean,
                    override var formula: InternalCNF)
  extends ADecisionLiteral() {

  var allNodes: mutable.Map[String, GraphNode] = mutable.Map[String, GraphNode]()

  def addNode(node: GraphNode): Unit = {
    allNodes += (InternalLiteral(node.varValue, node.varName).toString -> node)
  }

  def removeNode(node: GraphNode): Unit = {
    allNodes -= InternalLiteral(node.varValue, node.varName).toString
  }

  override def toModel: Map[String, Boolean] = {
    (allNodes map { case (_, v) => v.varName -> v.varValue }).toMap
  }
}

case class DecisionLiteral(override var varName: String,
                           override var varValue: Boolean,
                           override var formula: InternalCNF)
  extends ADecisionLiteral() {}

/**
  * NonDecisionLiteral
  *
  * @param varName  name of the variable
  * @param varValue value of the variable
  * @param decision the parent decision literal
  */
case class NonDecisionLiteral(override var varName: String,
                              override var varValue: Boolean,
                              decision: ADecisionLiteral)
  extends GraphNode() {}
