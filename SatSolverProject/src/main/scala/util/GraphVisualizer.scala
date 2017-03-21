package util

import java.io.{File, PrintWriter}

import sys.process._

/**
  * Generates Graph DOT files for visualizing the implication graph.
  */
object GraphVisualizer {

  val filename = "tmp.dot"

  private def toId(graphNode: GraphNode): String = {
    graphNode.toString.replace("!", "n")
  }

  private def getFormula(formula: InternalCNF): String = {
    if (formula == null) {
      ""
    } else {
      formula.toString
        .replace("Conjunction", "")
        .replace("Disjunction", "")
    }
  }

  def toDot(rootNode: RootNode): String = {

    var edges: Set[String] = Set()
    var nodes: Set[String] = Set()

    def _toDot(graphNode: GraphNode): Unit = {
      graphNode match {
        case d: RootNode =>
          nodes += toId(graphNode) + "[label=\""+ graphNode.toString + ":\n" + getFormula(d.formula) + "\", style=dashed, color=red]"
          d.decisionImplies
          .foreach(i => edges += toId(graphNode) + " -> " + toId(i) + " [color=grey, style=dotted]")
        case d: DecisionLiteral =>
          nodes += toId(graphNode) + "[label=\""+ graphNode.toString + ":\n" + getFormula(d.formula) + "\", style=dashed, color=red]"
          d.decisionImplies
          .foreach(i => edges += toId(graphNode) + " -> " + toId(i) + " [color=grey, style=dotted]")
        case _ =>
          nodes += toId(graphNode) + "[label=\""+ graphNode.toString + "\", color=blue]"
      }
      graphNode.children.foreach(c => {
        edges += toId(graphNode) + " -> " + toId(c) + {
          if (c.isInstanceOf[DecisionLiteral]) {
            "[style=dashed, color=red]"
          } else {
            "[color=blue]"
          }
        }
      })
      graphNode.children.foreach(c => _toDot(c))
      graphNode match {
        case d: DecisionLiteral => d.decisionImplies.foreach(c => _toDot(c))
        case r: RootNode => r.decisionImplies.foreach(c => _toDot(c))
        case _ =>
      }

    }

    _toDot(rootNode)
    "digraph my_graph  \n{" +
      nodes.foldLeft("")((acc, n) => acc + n + "\n")  + "\n" +
      edges.foldLeft("")((acc, e) => acc + e + "\n") + "\n}"
  }

  def displayXdot(rootNode: RootNode): Unit = {
    writeXdot(rootNode)
    "xdot " + filename !
  }

  def writeXdot(rootNode: RootNode): Unit = {
    val pw = new PrintWriter(new File(filename))
    pw.write(toDot(rootNode))
    pw.close()
  }

}
