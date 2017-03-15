package util

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
  var formula: InternalCNF

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
    val beforeCount = children.size
    children -= remove
    val afterCount = children.size
    if (afterCount != beforeCount - 1) {
      throw new IllegalStateException("Set of children of graph node did not shrink when removing one element! " +
        "Does the graph not contain the child?")
    }
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

  override def toString: String = "( " + varName + " = " + varValue.toString + " )"

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

case class RootNode(override var varName: String,
                    override var varValue: Boolean,
                    override var formula: InternalCNF)
  extends GraphNode() {}

case class DecisionLiteral(override var varName: String,
                           override var varValue: Boolean,
                           override var formula: InternalCNF)
  extends GraphNode() {}

case class NonDecisionLiteral(override var varName: String,
                              override var varValue: Boolean,
                              override var formula: InternalCNF)
  extends GraphNode() {}
