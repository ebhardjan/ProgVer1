package util

import org.scalatest.FunSuite

/**
  * Created by jan on 02.03.17.
  */
class CombinatorialTest extends FunSuite {

  test("calcCombinationsTest") {
    val ls = List(List(1, 2, 3), List(4, 5), List(6))
    val actual = Combinatorial.calcCombinations[Int](ls)
    val expected = List(
      List(1, 4, 6), List(2, 4, 6), List(3, 4, 6),
      List(1, 5, 6), List(2, 5, 6), List(3, 5, 6))
    assert(actual == expected)
  }

  test("calcCombinationsTestEmpty") {
    val ls = List()
    val actual = Combinatorial.calcCombinations[Int](ls)
    val expected = List()
    assert(actual == expected)
  }

  test("calcCombinationsTestSingleElement") {
    val ls = List(List(1), List(2), List(3))
    val actual = Combinatorial.calcCombinations[Int](ls)
    val expected = List(List(1, 2, 3))
    assert(actual == expected)
  }

  test("calcCombinationsTestDoubleElement") {
    val ls = List(List(1, 2), List(3, 4))
    val actual = Combinatorial.calcCombinations[Int](ls)
    val expected = List(List(1, 3), List(2, 3), List(1, 4), List(2, 4))
    assert(actual == expected)
  }

}
