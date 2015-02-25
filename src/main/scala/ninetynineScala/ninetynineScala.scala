package ninetynineScala

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

  def flatten(x: List[Any]): List[Any] = {
    
  }
}
