/*
* The marketing team is spending way too much time typing in hashtags.
Let's help them with our own Hashtag Generator!

Here's the deal:

It must start with a hashtag (#).
All words must have their first letter capitalized.
If the final result is longer than 140 chars it must return false.
If the input or the result is an empty string it must return false.
Examples
" Hello there thanks for trying my Kata"  =>  "#HelloThereThanksForTryingMyKata"
"    Hello     World   "                  =>  "#HelloWorld"
""                                        =>  false
* */

import scala.util.chaining._

def generateHashtag(s: String): String =
  s.split(" ").collect{
    case s if s.trim.nonEmpty => s.trim.head.toUpper + s.trim.tail.toLowerCase
  }.mkString.pipe(s => if (s.length > 140 || s.isEmpty) "" else "#" + s)

generateHashtag(" Hello there thanks for trying my Kata")
generateHashtag("    Hello     World   ")
generateHashtag("")