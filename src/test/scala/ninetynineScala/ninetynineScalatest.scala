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

  "Problem 4" should "find the number of elements in a list" in {
    ninetynineScala.length(List(1,1,2,3,5,8)) should be (6)
  }

  "Problem 5" should "reverse a list" in {
    reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }

  "Problem 6" should "find out whether a list is a palindrome" in {
    isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
    isPalindrome(List(1, 2, 2)) should be (false)
    isPalindrome(List(1, 2, 2, 1)) should be (true)
    isPalindrome(List()) should be (true)
  }

  "Problem 7" should "flatten a nested list structure" in {
    flatten(List(1,2,List(3,4))) should be (List(1,2,3,4))
    flatten(List(1,2,List(List(List(3))),List(4,5))) should be (List(1,2,3,4,5))
  }

  "Problem 8" should "eliminate consecutive duplicates of list elements" in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
    compress(List(1, 1, 2, 2, 3, 4, 3, 3, 3, 3, 3, 5, 5)) should be (List(1, 2, 3, 4, 3, 5))
  }

  "Problem 9" should "pack consecutive duplicates of list elements into sublists" in {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  "Problem 10" should "give run-length encoding of a list" in {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "Problem 11" should "do modified run-length encoding" in {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }
}
