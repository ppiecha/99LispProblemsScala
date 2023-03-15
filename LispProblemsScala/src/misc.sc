//class A(var n: Int)
//
//val a = new A(2)
//a.n
//a.n = 1
//a.n

abstract class Type
case class IntType(n: Int) extends Type
case class StringType(s: String) extends Type
case class FloatType(f: Double) extends Type

def check: Type => String = {
  case e: IntType => e.n.toString
  case e: StringType => e.s
  case e: FloatType => e.f.toString
}

check(IntType(2))
check(StringType("s"))
check(FloatType(3.14))