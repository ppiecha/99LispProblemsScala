/*
* P33 (*) Determine whether two positive integer numbers are coprime.
Two numbers are coprime if their greatest common divisor equals 1.
Example:
* (coprime 35 64)
T
* */

def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

def coprime(a: Int, b: Int): Boolean = gcd(a, b) == 1

coprime(35, 64)

/*
* P34 (**) Calculate Euler's totient function phi(m).
Euler's so-called totient function phi(m) is defined as
* the number of positive integers r (1 <= r < m) that are coprime to m.
Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

* (totient-phi 10)
4

Find out what the value of phi(m) is if m is a prime number.
* Euler's totient function plays an important role
* in one of the most widely used public key cryptography methods (RSA).
* In this exercise you should use the most primitive method to calculate this function
* (there are smarter ways that we shall discuss later).
* */

def totientPhi(m : Int): Int = m match {
  case 1 => 1
  case m => (1 until m).count(coprime(_, m))
}

totientPhi(10)