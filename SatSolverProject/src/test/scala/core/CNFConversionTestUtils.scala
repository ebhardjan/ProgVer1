package core

import java.io.{File, PrintWriter}

import smtlib.parser.Commands.Command
import smtlib.parser.Terms.{QualifiedIdentifier, SimpleIdentifier, Term}
import smtlib.theories.Core._
import util.PropositionalLogic

import sys.process._

/**
  * Created by jan on 03.03.17.
  *
  * Util methods for the CNFConversion tests
  */
object CNFConversionTestUtils {

  /**
    * reads an Smt2 file from the given filename and returns the formula
    */
  def readSmt2File(folder: String, filename: String): Term = {
    val path = "src/test/resources/cnf_conversion/" + folder + "/" + filename + ".smt2"
    val inputString = {
      val source = scala.io.Source.fromFile(path)
      try source.mkString finally source.close()
    }

    val script: List[Command] = MySATSolver.parseInputString(inputString)
    val (_, formula) = util.InputProcessing.processCommands(script)

    formula
  }

  /**
    * returns the cnf formula from a given file name
    */
  def getCnfFromFile(filename: String): Term = {
    val nonCnfFormula = readSmt2File("", filename)
    CNFConversion.toCNF(nonCnfFormula)
  }

  /**
    * Writes a given formula to the file tmp.smt2
    */
  def writeSmt2File(formula: Term): String = {
    if(!PropositionalLogic.isPropositional(formula)){
      throw new IllegalArgumentException("Only proposition formulas are supported!")
    }

    val variableNames = getVariableNamesFromFormula(formula)

    val pw = new PrintWriter(new File("tmp.smt2" ))

    pw.write("(set-option :produce-models true)\n" +
      "(set-logic QF_UF)\n")
    variableNames.foreach(n => pw.write("(declare-fun " + n + "() Bool)\n"))
    pw.write("(assert\n")
    pw.write(formula.toString)
    pw.write("\n)\n")
    pw.write("(check-sat)\n")

    pw.close()
    "tmp.smt2"
  }

  /**
    * Returns the names of the variables in the formula
    */
  def getVariableNamesFromFormula(formula: Term): Seq[String] = {
    {
      formula match {
        case True() | False() => Seq()
        case QualifiedIdentifier(SimpleIdentifier(id), _) => Seq(id.name)
        case Not(f) => getVariableNamesFromFormula(f)
        case Or(disjuncts@_*) => disjuncts.flatMap(d => getVariableNamesFromFormula(d))
        case And(conjuncts@_*) => conjuncts.flatMap(c => getVariableNamesFromFormula(c))
        case Implies(f,g) => getVariableNamesFromFormula(f) ++ getVariableNamesFromFormula(g)
        case Equals(f,g) => getVariableNamesFromFormula(f) ++ getVariableNamesFromFormula(g)
        case _ => Seq()
      }
    }.distinct
  }

  /**
    * checks if two formulas are equal by running z3 solver
    */
  def formulasEqual(formula1: Term, formula2: Term): Boolean = {
    val one = And(formula1, Not(formula2))
    val filename1 = writeSmt2File(one)
    val res1 = "z3 -smt2 " + filename1 !!

    val two = And(Not(formula1), formula2)
    val filename2 = writeSmt2File(two)
    val res2 = "z3 -smt2 " + filename2 !!

    res1.contains("unsat") || res2.contains("unsat")
  }

}
