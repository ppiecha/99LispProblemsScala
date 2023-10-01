/*
* P32 (**) Determine the greatest common divisor of two positive integer numbers.
Use Euclid's algorithm.
Example:
* (gcd 36 63)
9
* */

def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

gcd(36, 63)