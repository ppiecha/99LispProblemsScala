/*
* In this kata you are required to, given a string, replace every letter with its position in the alphabet.

If anything in the text isn't a letter, ignore it and don't return it.

"a" = 1, "b" = 2, etc.

Example
alphabetPosition("The sunset sets at twelve o' clock.")
Should return "20 8 5 19 21 14 19 5 20 19 5 20 19 1 20 20 23 5 12 22 5 15 3 12 15 3 11" ( as a string )
* */

object Kata {
  def alphabetPosition(text: String): String =
    text.toLowerCase.map(_.toInt).map(_ - 96).collect{case n if 1 to 26 contains n => n}.mkString(" ")
}

Kata.alphabetPosition("The sunset sets at twelve o' clock.")
Kata.alphabetPosition(raw"\:klnigF[aaoIf[2euVD6m(dDstx|!yvcf!brC)qw&\ffpQ|Qhjz")
assert(Kata.alphabetPosition(raw"\:klnigF[aaoIf[2euVD6m(dDstx|!yvcf!brC)qw&\ffpQ|Qhjz") ==
  "11 12 14 9 7 6 1 1 15 9 6 5 21 22 4 13 4 4 19 20 24 25 22 3 6 2 18 3 17 23 6 6 16 17 17 8 10 26")