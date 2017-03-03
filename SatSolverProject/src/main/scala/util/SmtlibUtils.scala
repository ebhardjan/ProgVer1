package util

import smtlib.parser.Terms.Term
import smtlib.theories.Core.{And, Or}

/**
  * Created by jan on 03.03.17.
  *
  * Util methods for the external representation (smtlib)
  */
object SmtlibUtils {

  /**
    * returns true if the provided list of elements contains an Or()
    */
  def hasOrElement(elementList : Seq[Term]): Boolean = {
    elementList.exists({
      case Or(_*) => true
      case _ => false
    })
  }

  /**
    * returns true if the provided list of elements contains an And()
    */
  def hasAndElement(elementList : Seq[Term]): Boolean = {
    elementList.exists({
      case And(_*) => true
      case _ => false
    })
  }

  /**
    * either return an And() in case conjuncts has more than 1 elements, otherwise just return the one element
    */
  def newAndOrSingleLiteral(conjuncts: Seq[Term]): Term = {
    if (conjuncts.length > 1) {
      And(conjuncts)
    } else {
      conjuncts.head
    }
  }

  /**
    * either return an Or() in case disjuncts has more than 1 elements, otherwise just return the one element
    */
  def newOrOrSingleLiteral(disjuncts: Seq[Term]): Term = {
    if (disjuncts.length > 1) {
      Or(disjuncts)
    } else {
      disjuncts.head
    }
  }

}
