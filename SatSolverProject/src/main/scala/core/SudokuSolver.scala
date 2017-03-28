package core

import smtlib.parser.Terms.Term
import util.Smt2FileUtils
import scala.sys.process._

import scala.io.Source

/**
  * Created by jan on 26.03.17.
  */
object SudokuSolver {

  /**
    * solves the unsolved sudoku puzzle in the specified file with a given solver
    *
    * @param path   relative path to the input file
    * @param solver one of: z3, dpll, cdcl
    * @return 9 by 9 array representing the solved sudoku puzzle
    */
  def solveSudokuInFile(path: String, solver: String): Array[Array[Int]] = {
    val field: Array[Array[Int]] = readSudokuFromFile(path)
    solve(field, solver)
  }

  private def readSudokuFromFile(path: String): Array[Array[Int]] = {
    val res: Array[Array[Int]] = Array.fill(9)(Array.fill(9)(0))
    var i = 0
    for (line <- Source.fromFile(path).getLines) {
      res(i) = line.map(c => c.toString.toInt).toArray
      i += 1
    }
    res
  }

  /**
    * solves a given sudoku puzzle with a given solver
    *
    * @param sudokuGrid 9 by 9 array representing the sudoku puzzle, with zeros where not yet filled
    * @param solver     one of: z3, dpll, cdcl
    * @return 9 by 9 array representing the solved sudoku puzzle
    */
  def solve(sudokuGrid: Array[Array[Int]], solver: String): Array[Array[Int]] = {
    val formula: Term = fieldToFormula(sudokuGrid)
    Smt2FileUtils.writeFormulaToSmt2File(formula, "sudoku.smt2", getModel = true)
    solver match {
      case "z3" => solveWithZ3(formula)
      case "cdcl" => solveWithSolver(formula, CDCLSolverWrapper)
      case "dpll" => solveWithSolver(formula, new DPLLSolver)
      case "dp" => solveWithSolver(formula, DPSolverWrapper)
    }
  }


  private def solveWithZ3(formula: Term): Array[Array[Int]] = {
    val res = "z3 -smt2 " + "sudoku.smt2" !!

    var trueNumbers: Seq[String] = Seq()
    val resArr = res.split("\n").drop(2).dropRight(1)

    val varNameRegex = "(p[0-9]*)".r
    val valNameRegex = "(true|false)".r

    for (i <- 0 to (resArr.length - 1) / 2) {
      val name = varNameRegex.findFirstIn(resArr(2 * i)).get
      val value = valNameRegex.findFirstIn(resArr(2 * i + 1)).get
      if (value.equals("true")) {
        trueNumbers = trueNumbers :+ name
      }
    }
    toArrayRepr(trueNumbers)
  }

  private def solveWithSolver(formula: Term, solver: SATSolvingAlgorithm): Array[Array[Int]] = {
    val cnf = CNFConversion.toCNF(formula)
    val res = solver.checkSAT(cnf).get
    var trueNumbers: Seq[String] = Seq()
    for ((k: String, v: Boolean) <- res) {
      if (v) {
        trueNumbers = trueNumbers :+ k
      }
    }
    toArrayRepr(trueNumbers)
  }


  private def toArrayRepr(trueNumbers: Seq[String]): Array[Array[Int]] = {
    val res: Array[Array[Int]] = Array.fill(9)(Array.fill(9)(0))
    trueNumbers.foreach(s => {
      val x: Int = s.charAt(1).toString.toInt
      val y: Int = s.charAt(2).toString.toInt
      val i: Int = s.charAt(3).toString.toInt
      res(x - 1)(y - 1) = i
    })
    res
  }

  /**
    * prints a given sudoku puzzle on the command line
    */
  def printIt(field: Array[Array[Int]]): Unit = {
    field.foreach(row => {
      row.foreach(f => print(f))
      println("")
    })
  }

  /**
    * Encodes a given field (2d array of integers) into a propositional logic formula.
    * We used the minimal encoding from here:
    * https://pdfs.semanticscholar.org/3d74/f5201b30772620015b8e13f4da68ea559dfe.pdf
    *
    * @param field 2d array of integers that represents the 9x9 grid
    * @return formula
    */
  private def fieldToFormula(field: Array[Array[Int]]): Term = {
    var formula: String = ""
    var vars: Seq[String] = Seq()

    formula += "(and"

    // encode the given numbers
    for (x <- 0 to 8) {
      for (y <- 0 to 8) {
        if (field(x)(y) != 0) {
          formula += " " + toVarName(x + 1, y + 1, field(x)(y)) + " "
        }
      }
    }

    // there is at least one number in each entry
    formula += "(and "
    for (x <- 1 to 9) {
      formula += "(and "
      for (y <- 1 to 9) {
        formula += " (or "
        for (z <- 1 to 9) {
          formula += toVarName(x, y, z) + " "
          vars = vars :+ toVarName(x, y, z)
        }
        formula += ")"
      }
      formula += ")"
    }
    formula += ")"

    // each number appears at most once in each row
    formula += " (and "
    for (y <- 1 to 9) {
      formula += " (and "
      for (z <- 1 to 9) {
        formula += " (and "
        for (x <- 1 to 9) {
          if (x + 1 <= 9) {
            formula += " (and "
            for (i <- (x + 1) to 9) {
              val s = f"(or (not ${toVarName(x, y, z)}) (not ${toVarName(i, y, z)}))"
              formula += s
            }
            formula += ")"
          }
        }
        formula += ")"
      }
      formula += ")"
    }
    formula += ")"

    // each number appears at most once in each column
    formula += "(and "
    for (x <- 1 to 9) {
      formula += "(and "
      for (z <- 1 to 9) {
        formula += " (and "
        for (y <- 1 to 9) {
          if (y + 1 <= 9) {
            formula += " (and "
            for (i <- y + 1 to 9) {
              formula += f"(or (not ${toVarName(x, y, z)}) (not ${toVarName(x, i, z)}))"
            }
            formula += ")"
          }
        }
        formula += ")"
      }
      formula += ")"
    }
    formula += ")"

    // each number appears at most once in each 3x3 sub-grid:
    formula += " (and"
    for (z <- 1 to 9) {
      formula += "(and "
      for (i <- 0 to 2) {
        formula += " (and "
        for (j <- 0 to 2) {
          formula += " (and "
          for (x <- 1 to 3) {
            formula += " (and "
            for (y <- 1 to 3) {
              if (y + 1 <= 3) {
                formula += " (and "
                for (k <- y + 1 to 3) {
                  formula += f"(or (not ${toVarName(3 * i + x, 3 * j + y, z)}) (not ${toVarName(3 * i + x, 3 * j + k, z)}))"
                }
                formula += ")"
              }
            }
            formula += ")"
          }
          formula += ")"
        }
        formula += ")"
      }
      formula += ")"
    }
    formula += ")"

    // each number appears at most once in each 3x3 sub-grid:
    formula += " (and"
    for (z <- 1 to 9) {
      formula += "(and "
      for (i <- 0 to 2) {
        formula += " (and "
        for (j <- 0 to 2) {
          formula += " (and "
          for (x <- 1 to 3) {
            if (x + 1 <= 3) {
              formula += " (and "
              for (y <- 1 to 3) {
                formula += " (and "
                for (k <- x + 1 to 3) {
                  formula += " (and "
                  for (l <- 1 to 3) {
                    formula += f"(or (not ${toVarName(3 * i + x, 3 * j + y, z)}) (not ${toVarName(3 * i + k, 3 * j + l, z)}))"
                  }
                  formula += ")"
                }
                formula += ")"
              }
              formula += ")"
            }
          }
          formula += ")"
        }
        formula += ")"
      }
      formula += ")"
    }
    formula += ")"

    formula += ")"
    val declarationsString = Smt2FileUtils.smt2Declarations(vars)
    val smt2String = Smt2FileUtils.createSmt2String(declarationsString, formula, getModel = true)
    Smt2FileUtils.smt2StringToFormula(smt2String)
  }

  private def toVarName(x: Int, y: Int, i: Int): String = {
    f"p$x$y$i"
  }

}
