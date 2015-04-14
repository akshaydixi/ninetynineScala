package ninetynineScala

import scala.annotation.tailrec
import scala.util.Random

object ninetynineScala {
  def last[A](x: List[A]): A = {
    x match {
      case Nil => throw new IllegalArgumentException("List should not be empty")
      case a :: Nil => a
      case a :: b => last(b)
    }
  }

  def penultimate[A](x: List[A]): A = {
    x match {
      case a :: b :: Nil => a
      case a :: Nil => throw new IllegalArgumentException("List should be of atleast 2 elements")
      case Nil => throw new IllegalArgumentException("List should be of atleast 2 elements")
      case a :: t => penultimate(t)
    }
  }

  def nth[A](x: List[A], k: Int): A = {
    if (k >= x.length) throw new IllegalArgumentException("k should be less than length of list")
    if (x == Nil) throw new IllegalArgumentException("List cannot be empty")
    k match {
      case 0 => x.head
      case k: Int => nth(x.tail, k - 1)
    }
  }

  def length[A](x: List[A]): Int = {
    x match {
      case Nil => 0
      case a :: b => 1 + length(b)
    }
  }

  def reverse[A](x: List[A]): List[A] = {
    x match {
      case Nil => Nil
      case a :: b => reverse(b) ::: List(a)
    }
  }

  def isPalindrome[A](x: List[A]): Boolean = x == reverse(x)

  def flatten[A](x: List[Any]): List[A] = {
    x match {
      case Nil          => List()
      case head :: tail => if (head.isInstanceOf[List[A]]) flatten(head.asInstanceOf[List[A]]) ::: flatten(tail)
                           else List(head.asInstanceOf[A]) ::: flatten(tail)
    }
  }

  def compress[A](x: List[A]): List[A] = {
    x match {
      case Nil => List()
      case a :: Nil => List(a)
      case a :: b :: tail => if (a == b) compress(b :: tail)
                             else a :: compress(b :: tail)
    }
  }

  def pack[A](x: List[A]): List[List[A]] = {
      def packer(x: List[A], result: List[A], acc: List[List[A]]) : List[List[A]] = {
        if (x.isEmpty) acc ::: List(result)
        else if (x.head == result.head) packer(x.tail, x.head :: result, acc)
        else packer(x.tail, List(x.head), acc ::: List(result))
      }

      x match {
        case Nil => Nil
        case x: List[A] => packer(x.tail, List(x.head), List())
      }
  }

  def encode[A](x: List[A]): List[(Int, A)] = {
    def f(x: List[A]): (Int, A) = (length(x),x.head)
    pack(x).map(f)
  }

  def encodeModified[A](x: List[A]): List[Any] = {
    encode(x).map(l => if(l._1 == 1) l._2 else l )
  }

  def expand[A](n: Int, x: A): List[A] = {
    if (n == 0) List()
    else x :: expand(n-1, x)
  }

  def decode[A](x: List[(Int, A)]): List[A] = {
    def decodeIter(x: List[(Int, A)], acc: List[A]): List[A] = {
      if (ninetynineScala.length(x) == 0) acc
      else decodeIter(x.tail, acc ++ expand(x.head._1, x.head._2))
    }
    decodeIter(x, List())
  }

  def duplicate[A](x: List[A]): List[A] = {
    x match {
      case Nil => Nil
      case x: List[A] => List(x.head, x.head) ++ duplicate(x.tail)
    }
  }

  def duplicateN[A](N: Int, x: List[A]): List[A] = {
    def Nfrom1(n: Int, one: A): List[A] = {
      n match {
        case 0 => Nil
        case positive: Int => List(one) ++ Nfrom1(positive - 1,one)
      }
    }
    x match {
      case Nil => Nil
      case x: List[A] => Nfrom1(N,x.head) ++ duplicateN(N, x.tail)
    }
  }

  def drop[A](N: Int, x: List[A]): List[A] = {
    (x zip (Stream from 1)).filter(a => a._2 % N != 0).unzip._1
  }

  def split[A](N: Int, x: List[A]): (List[A],List[A]) = {
    @tailrec
    def itersplit(N: Int, x: List[A], y: List[A]): (List[A], List[A]) = {
      if (N == 0) (reverse(y),x)
      else itersplit(N-1, x.tail, x.head :: y)
    }
    itersplit(N, x, List())
  }

  def slice[A](I: Int, K: Int, x: List[A]): List[A] = {
    split(K-I, split(I,x)._2)._1
  }

  def rotate[A](N: Int, x: List[A]): List[A] = {
    if (N < 0) {
      val tmp = split( length(x) + N , x)
      tmp._2 ++ tmp._1
    } else {
      val tmp = split( N , x)
      tmp._2 ++ tmp._1
    }
  }

  def removeAt[A](K: Int, x: List[A]): (List[A], A) = {
    def removeAtIter(K: Int, prev: List[A], next: List[A]) : (List[A], A) = {
      if (K == 0) (prev ++ next.tail,next.head)
      else removeAtIter(K - 1, prev ::: List(next.head), next.tail )
    }
    removeAtIter(K, List(), x)
  }

	def insertAt[A](element: A, K: Int, x: List[A]): List[A] = {
		val splitted = split(K,x)
		splitted._1 ++ List(element) ++ splitted._2
	}

	def range(start: Int, end: Int) : List[Int] = {
		if (start > end) range(end, start)
		if (start == end) List(end)
		else start::range(start+1,end)
	}

	def randomSelect[A](K: Int, x: List[A]): List[A] = {
		if (K == 0) return List()
		val randomNumber = new Random().nextInt(length(x))
		val removal = removeAt(randomNumber, x)
		removal._2 :: randomSelect(K-1, removal._1)
	}

	def lotto(N: Int, M:Int): List[Int] = {
		randomSelect(N,range(1,M))
	}

	def randomPermute[A](x: List[A]): List[A] = {
		randomSelect(x.length, x)
	}
}
