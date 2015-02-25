package ninetynineScala

import org.scalatest._
import ninetynineScala._

class ninetynineScalatest extends FlatSpec with Matchers {

  "Problem 1" should "return last element of a list" in {
    last(List(1,2,3,4,5)) should be (5)
    last(List("a","b","c")) should be ("c")
    last("Pokemon".toList) should be ('n')
  }

  it should "throw IllegalArgumentException if an empty list is given" in {
    a [IllegalArgumentException] should be thrownBy {
      last(List())
    }
  }

  "Problem 2" should "return the last but one element of the list" in {
    penultimate(List(1,2,3,4,5)) should be (4)
    penultimate(List("a","b","c")) should be ("b")
    penultimate("Pokemon".toList) should be ('o')
  }

  it should "throw IllegalArgumentException if a list of size < 2 is given" in {
    a [IllegalArgumentException] should be thrownBy {
      penultimate(List())
      penultimate(List(1))
    }
  }

  "Problem 3" should "return the kth element of the list" in {
    nth(List(1,1,2,3,5,8),2) should be (2)
  }
}
