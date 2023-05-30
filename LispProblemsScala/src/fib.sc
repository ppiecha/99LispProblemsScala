def fib(n: Int): BigInt = {
  var cache = Map[Int, BigInt](0 -> 0, 1 -> 1)
  def fib_cached(n: Int): BigInt = n match {
    case 0 | 1 => cache(n)
    case _ => if (cache contains n) cache(n) else {
      val fib_n = fib_cached(n - 2) + fib_cached(n - 1)
      cache += (n -> fib_n)
      fib_n
    }
  }
  fib_cached(n)
}

fib(50)
fib(60)
fib(70)