package core

import java.io.File

import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, TimeoutException}

/**
  * Created by jan on 26.03.17.
  *
  * In these tests we compare the result of the z3 solver with the result of our solver.
  * So we do not test the encoding here, just whether our solver can handle the formulas and the results are the same as
  * the z3 solver provides.
  */
class SudokuSolverTest extends FunSuite {

  val directory = "src/test/resources/sudoku/"
  val solver = "dpll"
  val maxRuntime: FiniteDuration = 1 minute

  getListOfSudokuFiles(directory).foreach(file => {
    test(file) {
      val f = SudokuSolver.solveSudokuInFile(directory + file, "z3")

      try {
        val g = Await.result(Future[Array[Array[Int]]] {
          SudokuSolver.solveSudokuInFile(directory + file, solver)
        }, maxRuntime)
        assertEquals(f, g)
      }
      catch {
        case _: TimeoutException =>
          fail(f"file $file timed out after $maxRuntime!")
      }
    }
  })

  private def getListOfSudokuFiles(directoryPath: String): List[String] = {
    val directory = new File(directoryPath)
    if (directory.exists && directory.isDirectory) {
      directory.listFiles
        .filter(f => f.isFile)
        .filter(f => f.getName.endsWith(".txt"))
        .map(f => f.getName)
        .toList
    } else {
      List()
    }
  }

  private def assertEquals(a1: Array[Array[Int]], a2: Array[Array[Int]]): Unit = {
    for (i <- 0 to 8) {
      for (j <- 0 to 8) {
        assert(a1(i)(j) == a2(i)(j))
      }
    }
  }

}
