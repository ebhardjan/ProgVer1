package core

import CNF2SMTLIB.CNF2SMTLIBv2Converter
import smtlib.parser.Commands._
import util.{PropositionalLogic, Utils}

// A Scala "object" is a singleton object. Here, we merely use it to hold, in the Java-sense, static methods,
// by declaring them inside the object.
object MySATSolver {

  private def solveSudoku(args: Array[String]): Unit = {
    val algoString = {
      args.length match {
        case 2 => "/dpll"
        case 3 => args(1)
        case _ => abortExecution("Invalid arguments.")
      }
    }

    val solvedSudoku: Array[Array[Int]] = SudokuSolver.solveSudokuInFile(args(0), algoString.replace("/", ""))
    SudokuSolver.printIt(solvedSudoku)
  }

  def main(args: Array[String]) {
    // check that a command-line argument was passed (which will be treated as the input String)
    if(args.length == 0 || args.length > 4) {
      abortExecution("Invalid arguments. Need 1 to 4 arguments:\n" +
        "[/cnf]: add this if the file is in .cnf format\n" +
        "<file>: path to the file containing the formula to check (in .smt2 or .cnf format)\n" +
        "[/dp][/dpll][/cdcl]: add one of those to specify the algorithm to be used in solving.\n" +
        "[/sudoku]: if the the file contains a sudoku puzzle that should be solved with de previously specified algorithm.\n")
    }

    // Just run the evaluation and exit the program.
    if(args.length == 3 && args(0) == "/eval") {
      println(AlgorithmEvaluator.runExperiments(args(1), args(2).replace("/", "")))
      System.exit(0)
    }

    val isSudoku = {
      args.length match {
        case 1 => false
        case 2 => args(1) == "/sudoku"
        case 3 => args(2) == "/sudoku"
        case 4 => false
      }
    }

    if (isSudoku) {
      solveSudoku(args)
      return
    }

    val inputString = {
      if(args(0) == "/cnf") {
        convertDIMACSFileToSMTLIBv2(args(1))
      } else {
        // This pattern is typical for reading an entire text file into a String
        val source = scala.io.Source.fromFile(args(0))
        try source.mkString finally source.close() // make sure the file is closed
      }
    }

    val script: List[Command] = parseInputString(inputString)

    // extract the list of identifiers and input formula from the list of smtlib commands
    val (declarations, formula) = util.InputProcessing.processCommands(script)

    // Now, we have the list of function declarations (corresponding to the variables in the input problem),
    // and the formula to check for satisfiability

    // The message is formatted using string interpolation (requires strings of the shape s"...").
    // To splice the result of a more complex Scala expression into a string, surround it with curly braces, e.g. s"1 > ${1 + 1}".
    assert(PropositionalLogic.isPropositional(formula),
           s"The parsed formula is not a propositional formula; got: $formula")

    val CNFInput = CNFConversion.toCNF(formula)

    // Pick algorithm specified in input
    // use DPLL as default
    val defaultAlgo = "/dpll"
    val algoString = {
      args.length match {
        case 1 => defaultAlgo
        case 2 => if (args(0) != "/cnf") args(1) else defaultAlgo
        case 3 => args(2)
      }
    }
    val solver: SATSolvingAlgorithm = {
      algoString match {
        case "/dp" => new DPSolver
        case "/dpll" => new DPLLSolver
        case "/cdcl" => new CDCLSolver
        case _ => abortExecution(s"Invalid algorithm selection: $algoString")
      }
    }

    // do the sat checking and print result
    val res = solver.checkSAT(CNFInput)
    println(solver.outputResult(res))
  }


  // A Scala method that, unlike method main above, has a return value
  def parseInputString(input : String) : List[Command] = {
    // make use of the pre-existing smtlib Lexer and Parser
    val lexer = new smtlib.lexer.Lexer(new java.io.StringReader(input)) // Java libraries and code can be used interchangeably in Scala code
    val parser = new smtlib.parser.Parser(lexer)

    // Scala has both immutable and mutable collection libraries. Here a mutable one makes more sense (since we build it up incrementally), but either could be used.
    // Typically, a mutable collection is used with an immutable local variable (val), whereas an immutable collection is used with a mutable local variable (var).
    val cmds = new scala.collection.mutable.ListBuffer[Command]
    var cmd = parser.parseCommand // try to parse the next command in the input SMTlib string
    while(cmd != null) { // keep parsing all of the commands into a list of smtlib.parser.Commands.Command instances
      cmds.append(cmd)
      cmd = parser.parseCommand
    }

    cmds.toList // In Scala, the value of the last statement in a block is the value of the block (here, the return value of the parseScript method)
  }





  // Used to abort execution of the program - we could alternatively use exceptions (which work exactly as in Java) for this
  def abortExecution(reason: String) : Nothing = { // Nothing is a type that no value will ever have; a method returning "Nothing" is guaranteed not to return normally (in this case, because it exits). "Nothing" is a subtype of all types, allowing calls to this method to type-check in any position.
    println(reason) // print or println are useful for debugging, too
    sys.exit(1) // abort the program with exit code 1
  }




  // Helper method for converting SAT input files into SMT-LIB format.
  // You probably won't need this method, at least initially.
  // This method will (only) be useful if you want to test your implementation against SAT benchmarks that you find online (these are typically in DIMACS format, rather than SMTlib)
  def convertDIMACSFileToSMTLIBv2(fileInDIMACSFormat : String) : String = {
    val converter = new CNF2SMTLIBv2Converter // parentheses are generally not needed for calls which take no parameters
    converter.convertDimacs(fileInDIMACSFormat)
  }
}
