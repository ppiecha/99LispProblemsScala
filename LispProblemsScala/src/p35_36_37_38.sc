import scala.math.sqrt
/*
* P35 (**) Determine the prime factors of a given positive integer.
Construct a flat list containing the prime factors in ascending order.
Example:
* (prime-factors 315)
(3 3 5 7)
* */

def isPrime(n: Int): Boolean = (2 to sqrt(n).floor.toInt).forall(x => n % x != 0)

def primeNumbers(n: Int): List[Int] = (2 until n).toList.filter(isPrime)

primeNumbers(20)

def primeFactors(n: Int): List[Int] = {
  def loop(n: Int, primes: List[Int]): List[Int] = (n, primes) match {
    case (_, Nil) => Nil
    case (n, h :: t) =>
      if (n % h == 0) h :: loop(n / h, h :: t)
      else loop(n, t)
  }
  loop(n, primeNumbers(n))
}

primeFactors(315)

/*
* P36 (**) Determine the prime factors of a given positive integer (2).
Construct a list containing the prime factors and their multiplicity.
Example:
* (prime-factors-mult 315)
((3 2) (5 1) (7 1))
Hint: The problem is similar to problem P10.
* */

def primeFactorsMult(n: Int): List[(Int, Int)] =
  primeFactors(n).groupBy(x => x).toList.map(pair => (pair._1, pair._2.length)).sortBy(_._1)

primeFactorsMult(315)

/*
* P37 (**) Calculate Euler's totient function phi(m) (improved).
See problem P34 for the definition of Euler's totient function.
* If the list of the prime factors of a number m is known in the form of problem P36
* then the function phi(m) can be efficiently calculated as follows:
* Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m.
* Then phi(m) can be calculated with the following formula:
phi(m) = (p1 - 1) * p1 ** (m1 - 1) * (p2 - 1) * p2 ** (m2 - 1) * (p3 - 1) * p3 ** (m3 - 1) * ...

Note that a ** b stands for the b'th power of a.
* */

def phi(m: Int): Int = primeFactorsMult(m).foldLeft(1){
  case (acc, (n, count)) => acc * (n - 1) * scala.math.pow(n, count - 1).toInt
}

phi(10)