package core

import CNF2SMTLIB.CNF2SMTLIBv2Converter
import smtlib.parser.Commands._
import util.{PropositionalLogic, Utils}

// A Scala "object" is a singleton object. Here, we merely use it to hold, in the Java-sense, static methods,
// by declaring them inside the object.
object MySATSolver {

  // This is a Scala method declaration: it declares a method with a single argument and no return value (void in Java)
  def main(args: Array[String]) {
    // check that a command-line argument was passed (which will be treated as the input String)

    // You will need to extend the command-line handling code during your project
    if(args.length == 0) {
      abortExecution("No input file specified.")
    }

    if(args.length > 2) {
      abortExecution("Too many command-line arguments specified (expected 1 or 2)")
    }

    if(args.length == 2 && args(0) != "/cnf") {
      abortExecution(s"Unexpected command-line arguments: expected filename or /cnf filename, got: ${args(0)} ${args(1)}")
    }

    // "val" indicates immutable storage; we will never reassign inputString
    val inputString = // we can use a block of statements as an expression; its value will be the value of the last statement in the block
    {
      if(args.length == 1) {
        // This pattern is typical for reading an entire text file into a String
        val source = scala.io.Source.fromFile(args(0))
        try source.mkString finally source.close() // make sure the file is closed
      } else { // convert from .cnf (DIMACS) format
        convertDIMACSFileToSMTLIBv2(args(1))
      }
    }

    // type annotations on declarations (such as "List[Command]" here) are optional; the compiler will otherwise try to infer a type from the assigned right-hand-side expression
    // However, it might not always choose the type you expect, and sometimes adding the types explicitly can make code more readable.
    val script: List[Command] = parseInputString(inputString)

    // we can declare and assign pairs / tuples in Scala, rather than declaring each element separately
    val (declarations, formula) = util.InputProcessing.processCommands(script) // extract the list of identifiers and input formula from the list of smtlib commands

    // Now, we have the list of function declarations (corresponding to the variables in the input problem), and the formula to check for satisfiability

    // A runtime check: if the first parameter isn't true, the execution aborts with the (optional) second parameter as the exception message
    // The message is formatted using string interpolation (requires strings of the shape s"...").
    // To splice the result of a more complex Scala expression into a string, surround it with curly braces, e.g. s"1 > ${1 + 1}".
    assert(PropositionalLogic.isPropositional(formula),
           s"The parsed formula is not a propositional formula; got: $formula")


    // the real work should start here!

    val CNFInput = CNFConversion.toCNF(formula)
//    val res = DPSolver.checkSAT(CNFInput)
    val dPLLSolver = new DPLLSolver
    val res = Utils.time(dPLLSolver.checkSAT(CNFInput))
    println((new DPSolver).outputResult(res))
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
