package fp

object TestEquality extends App {

  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  implicit object IntEqual extends Equal[Int] {
    override def apply(a: Int, b: Int): Boolean = a == b
  }

  object Equal {
    def apply[T](a: T, b: T)(implicit instance: Equal[T]) = instance.apply(a, b)
  }

  /*
    Exercise - improve the Equal TC with an implicit conversion class
    ===(anotherValue: T)
    !==(anotherValue: T)
   */

  implicit class TypeSafeEqual[T](value: T) {
    def ===(anotherValue: T)(implicit e: Equal[T]) = e.apply(value, anotherValue)

    def !==(anotherValue: T)(implicit e: Equal[T]) = e.apply(value, anotherValue)
  }

  println(Equal(2 + 2, 4)(IntEqual))
  println(Equal(2 + 2, 4))
  println((2 + 2) === 4)
  println((2 + 2) !== 4)
  //  TypeSafeEqual[Int](2 + 2).===

  case class User(name: String)

  // implicitly
  implicit val defaultUser = User("John")
  val standardUser = implicitly[User]

  def imp[T](implicit t: T) = t
  println(imp[User])
}