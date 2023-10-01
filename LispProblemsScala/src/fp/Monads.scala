package fp

object Monads extends App {

  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  // m.unit(x).flatMap(f) = f(x)
  // Attempt(x).flatMap(f) = f(x)
  // x.flatMap(y => unit(y)) = x
  // Attempt(a).flatMap(y => unit(y)) = a
  // m.flatMap(f).flatMap(g) = m.flatMap(x => f(x).flatMap(g))
  // Attempt(a).flatMap(f).flatMap(g) = Attempt(a).flatMap(x => f(x).flatMap(g))
  // f(Success(a)).flatMap(g) = f(Success(a)).flatMap(g)

  class MyLazy[A](value: => A) {
    def flatMap[B](f: (=> A) => MyLazy[B]): MyLazy[B] = f(value)
  }

  object MyLazy {
    def apply[A](value: => A): MyLazy[A] = new MyLazy[A](value)
  }

  val lazyVal = MyLazy{
    println("executed")
    if (true) 1 else 0
  }
  // Monad flaMap[B](f: T => Monad[B]): Monad[B] - implemented
  // def map[B](f: T => B): Monad[B] = flatMap(x => Monad(f(x)))
  // def flatten(m: Monad[Monad[T]]): Monad[T] = m.flatMap(
}
