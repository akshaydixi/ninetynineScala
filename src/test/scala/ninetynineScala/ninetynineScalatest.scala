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

  "Problem 12" should "construct an uncompressed version from run-length code list" in {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "Problem 14" should "duplicate the elements of a list" in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "Problem 15" should "duplicate the elements of a list a given number of times" in {
    duplicateN(3,  List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  "Problem 16" should "drop every Nth element from the list" in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "Problem 17" should "split a list into two parts" in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be ((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  "Problem 18" should "extract a slice from a list" in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g))
  }

  "Problem 19" should "rotate a list N places to the left" in {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  "Problem 20" should "remove the Kth element from a list" in {
    removeAt(1, List('a, 'b, 'c, 'd)) should be ((List('a, 'c, 'd),'b))
  }

	"Problem 21" should "insert an element at a given position into a list" in {
		insertAt('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))
	}

	"Problem 22" should "create a list containing all integers within a given range" in {
		range(4, 9) should be (List(4, 5, 6, 7, 8, 9))
	}

	"Problem 25" should "generate a random permutation of elements of a list" in {
		randomPermute(List(3,4,5,2,1)).sorted should be (List(1,2,3,4,5))
	}

}
