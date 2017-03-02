package util

import scala.reflect.ClassTag

/**
  * Created by jan on 02.03.17.
  *
  * Utility class for combinatorial functions
  */
object Combinatorial {

  /**
    * Helper method that creates all possible combinations of a number of lists by picking one element of each list.
    *
    * - the order of the elements across the lists is preserved, first all elements of the first list are altered, then
    * of the second etc.
    * - example: calcCombinations[Int](List(List(1, 2), List(3, 4)) evaluates to List(List(1,3), List(2,3))
    */
  def calcCombinations[T: ClassTag](list: List[List[T]]): List[List[T]] = {
    list match {
      case Nil => List()
      case l :: Nil => l.map(e => List(e))
      case l :: ls =>
        val subCombinations = calcCombinations[T](ls)
        subCombinations.flatMap(c => l.map(lItem => lItem :: c))
    }
  }
}
