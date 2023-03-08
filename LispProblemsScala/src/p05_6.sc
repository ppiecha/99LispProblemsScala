// P05 (*) Reverse a list.

def reverse(list: List[Any]): List[Any] = list match {
  case Nil => Nil
  case x :: xs => reverse(xs) ::: List(x)
}

def palindrome(list: List[Any]): Boolean = list == reverse(list)

assert(reverse(List(1,2,3,4)) == List(4,3,2,1))

assert(palindrome(List("x", "a", "m", "a", "x")))
