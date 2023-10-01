import scala.math.sqrt

/*
* P39 (*) A list of prime numbers.
Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
* */

def isPrime(n: Int): Boolean = (2 to sqrt(n).floor.toInt).forall(x => n % x != 0)

isPrime(1)
isPrime(2)
isPrime(3)
isPrime(4)
isPrime(5)
isPrime(6)


def primeNumbers(n: Int, m: Int): List[Int] = (n to m).toList.filter(isPrime)

primeNumbers(10, 23)

