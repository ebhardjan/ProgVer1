package util

import smtlib.parser.Terms.{QualifiedIdentifier, SimpleIdentifier, Term}
import smtlib.theories.Core._

// Case classes are special Scala classes that are typically used similar to abstract data types in Haskell
//   - they are compared via structural equality: if all the arguments are equal, so are the case class instances
//   - they can be instantiated without 'new', e.g. 'InternalLiteral(true, "x")' instead of 'new InternalLiteral(true, "x")'
//   - they can be decomposed via pattern matching
//   - inheriting from a case class is *strongly* discouraged
case class InternalLiteral(polarity : Boolean, name : String) {
  def negation = InternalLiteral(!polarity, name)
  override def toString: String = (if (polarity) name else "!" + name)

  // returns the set of variable names occurring positively/negatively (according to b)
  def vars(b : Boolean) : Set[String] = if (b == polarity) Set(name) else Set()
}

// a var parameter to a case class is still included in the built-in notion of equality, but is a field which can be later modified
case class InternalDisjunct(literal: InternalLiteral, var isActive : Boolean) {
  override def toString: String = if (isActive) literal.toString else "[" + literal.toString + "]"

  // returns the set of variable names occurring positively/negatively (according to b)
  def vars(b: Boolean) : Set[String] = if (isActive) literal.vars(b) else Set()

  override def clone(): InternalDisjunct = {
    InternalDisjunct(literal, isActive)
  }
}

case class InternalClause(disjuncts : Set[InternalDisjunct]) {
  override def toString: String = "  Disjunction(" + (if (disjuncts.isEmpty) ")" else " " + disjuncts.tail.foldLeft(disjuncts.head.toString)((s, d) => s + " , " + d.toString) + " )")

  // returns the set of variable names occurring positively/negatively (according to b)
  def vars(b: Boolean) : Set[String] = disjuncts.foldLeft[Set[String]](Set())((set,d) => set union d.vars(b))

  override def clone(): InternalClause = {
    InternalClause(disjuncts.map(d => d.clone()))
  }

}

case class InternalCNF(conjuncts : Set[InternalClause]) {
  override def toString: String = "Conjunction(" + (if (conjuncts.isEmpty) ")" else "\n" + conjuncts.tail.foldLeft(conjuncts.head.toString)((s,c) => s + ",\n" + c.toString) + "\n)")

  // returns the set of variable names occurring positively/negatively (according to b)
  def vars(b: Boolean) : Set[String] = conjuncts.foldLeft[Set[String]](Set())((set,c) => set union c.vars(b))

  override def clone(): InternalCNF = {
    InternalCNF(conjuncts.map(c => c.clone()))
  }
}

// convert a given SMT-LIB formula (in CNF form) to its corresponding internal representation
object CNFRepresentation {

  def convertLiteralToInternalDisjunct(input: Term): InternalDisjunct = input match {
    case QualifiedIdentifier(SimpleIdentifier(s), _) => InternalDisjunct(InternalLiteral(true, s.name), true)
    case Not(QualifiedIdentifier(SimpleIdentifier(s), _)) => InternalDisjunct(InternalLiteral(false, s.name), true)
  }

  def convertClauseToInternal(input: Term): InternalClause = {
    val internalDisjuncts: Set[InternalDisjunct] = input match {
      case False() => Set() // represents empty clause / empty disjunction
      case Or(disjuncts@_*) => disjuncts.foldLeft[Set[InternalDisjunct]](Set())((set, d) => set + convertLiteralToInternalDisjunct(d))
      case _ => Set(convertLiteralToInternalDisjunct(input)) // only other allowed case is that the input is a single literal
    }
    InternalClause(internalDisjuncts)
  }

  def convertCNFToInternal(input: Term): InternalCNF = {
    val conjuncts: Set[InternalClause] = input match {
      case True() => Set() // empty conjunction
      case And(conjuncts@_*) => conjuncts.foldLeft[Set[InternalClause]](Set())((set, c) => set + convertClauseToInternal(c))
      case _ => Set(convertClauseToInternal(input)) // only other allowed case is that the input is a single clause
    }
    InternalCNF(conjuncts)
  }

}
