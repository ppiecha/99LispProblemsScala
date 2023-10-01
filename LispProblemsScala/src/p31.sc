/*
* P31 (**) Determine whether a given integer number is prime.
Example:
* (is-prime 7)
T
* */

def isPrime(n: Int): Boolean = (2 to n / 2).forall(x => n % x != 0)

isPrime(2)
isPrime(3)
isPrime(4)
isPrime(5)
isPrime(7)