package fp

sealed abstract class Option[+T] {
  def map[A]: (T => A) => Option[A]
  def flatMap[A]: (T => List[A]) => List[Option[A]]
  //def flatMap[A]: (T => Option[A]) => Option[A]
  def filter: (T => Boolean) => Option[T]
}

case object None extends Option[Nothing] {
  override def map[A]: (Nothing => A) => Option[A] = _ => None

  def flatMap[A]: (Nothing => List[A]) => List[Option[A]] = _ => List[Nothing]()

  //def flatMap[A]: (Nothing => Option[A]) => Option[A] = _ => None

  override def filter: (Nothing => Boolean) => Option[Nothing] = _ => None
}

case class Some[T](value: T) extends Option[T] {
  override def map[A]: (T => A) => Option[A] = mapper => Some(mapper(value))

  override def flatMap[A]: (T => List[A]) => List[Option[A]] = mapper => mapper(value) match {
    case List() => List()
    case x :: xs => Some(x) :: xs.map(Some(_))
    }

  //override def flatMap[A]: (T => Option[A]) => Option[A] = mapper => mapper(value)

  override def filter: (T => Boolean) => Option[T] = predicate =>
    if (predicate(value)) Some(value)
    else None
}

object MyOption extends App{
  val someInt = Some(2)
  println(someInt.map(x => (x + 2).toString))
  println(someInt.flatMap{x => List(x, x + 1)})
  println(someInt.filter(_ % 2 == 0))
  println(someInt.filter(_ % 2 == 1))
}
