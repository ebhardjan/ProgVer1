package util

/**
  * Created by jan on 12.03.17.
  *
  * Graph representation used in the CDCL algorithm.
  */
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
      case RootNode(_, _, _) => Map()
      case NonDecisionLiteral(vN, vV, _) => Map(vN -> vV)
      case DecisionLiteral(vN, vV, _) => Map(vN -> vV)
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
