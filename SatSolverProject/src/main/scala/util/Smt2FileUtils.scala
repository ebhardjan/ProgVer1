package util

import java.io.{File, PrintWriter}

import core.MySATSolver
import smtlib.parser.Commands.Command
import smtlib.parser.Terms.{QualifiedIdentifier, SimpleIdentifier, Term}
import smtlib.theories.Core._

/**
  * Created by jan on 23.03.17.
  */
object Smt2FileUtils {

  private[this] val preamble: String = "(set-option :produce-models true)\n" +
    "(set-logic QF_UF)\n"

  private[this] def commands(getModel: Boolean): String = {
    if (getModel) {
      "(check-sat)\n(get-model)\n"
    } else {
      "(check-sat)\n"
    }
  }

  /**
    * creates a formula given an smt2 string
    */
  def smt2StringToFormula(inputString: String): Term = {
    val script: List[Command] = MySATSolver.parseInputString(inputString)
    val (_, formula) = util.InputProcessing.processCommands(script)
    formula
  }

  /**
    * reads an Smt2 file from the given filename and returns the formula
    */
  def readSmt2File(folder: String, filename: String): Term = {
    val path = folder + "/" + filename + ".smt2"
    val inputString = {
      val source = scala.io.Source.fromFile(path)
      try source.mkString finally source.close()
    }
    smt2StringToFormula(inputString)
  }

  /**
    * Creates an smt2 string for a formula
    */
  def formulaToSmt2String(formula: Term, getModel: Boolean): String = {
    if (!PropositionalLogic.isPropositional(formula)) {
      throw new IllegalArgumentException("Only proposition formulas are supported!")
    }

    val declarationString = smt2Declarations(getVariableNamesFromFormula(formula))
    createSmt2String(declarationString, formula.toString, getModel)
  }

  /**
    * Writes a given formula to a .smt2 file
    *
    * @param formula formula to write to the file
    * @param filepath where to store the formula, if the directory does not exist, it is created
    * @param getModel whether the smt2 file should contain the (get-model) command or not
    * @return the path of the
    */
  def writeFormulaToSmt2File(formula: Term, filepath: String, getModel: Boolean): Unit = {
    createDirectoryForFilePathIfNotExists(filepath)
    val pw = new PrintWriter(new File(filepath))
    pw.write(formulaToSmt2String(formula, getModel))
    pw.close()
  }

  private def createDirectoryForFilePathIfNotExists(filepath: String): Unit = {
    val lastIndexOfDirectorySubString = filepath.lastIndexOf("/")
    if (lastIndexOfDirectorySubString > 0) {
      val dirName = filepath.substring(0, lastIndexOfDirectorySubString)
      val directory = new File(dirName)
      if (!directory.exists()) {
        directory.mkdir()
      }
    }
  }

  /**
    * Returns the names of the variables in the formula
    */
  private[this] def getVariableNamesFromFormula(formula: Term): Seq[String] = {
    {
      formula match {
        case True() | False() => Seq()
        case QualifiedIdentifier(SimpleIdentifier(id), _) => Seq(id.name)
        case Not(f) => getVariableNamesFromFormula(f)
        case Or(disjuncts@_*) => disjuncts.flatMap(d => getVariableNamesFromFormula(d))
        case And(conjuncts@_*) => conjuncts.flatMap(c => getVariableNamesFromFormula(c))
        case Implies(f, g) => getVariableNamesFromFormula(f) ++ getVariableNamesFromFormula(g)
        case Equals(f, g) => getVariableNamesFromFormula(f) ++ getVariableNamesFromFormula(g)
        case _ => Seq()
      }
    }.distinct
  }

  /**
    * Adds the preamble, declarations and commands. Wraps the formula into an assert
    */
  def createSmt2String(declarationsString: String, formulaString: String, getModel: Boolean): String = {
    val sb = StringBuilder.newBuilder

    sb.append(preamble)
    sb.append(declarationsString)
    sb.append("(assert\n")
    sb.append(formulaString)
    sb.append("\n)\n")
    sb.append(commands(getModel))

    sb.toString()
  }

  /**
    * returns the smt2 declarations from a given list of variable names
    */
  def smt2Declarations(variableNames: Seq[String]): String = {
    val sb = StringBuilder.newBuilder
    variableNames.foreach(n => sb.append("(declare-fun " + n + "() Bool)\n"))
    sb.toString()
  }

}