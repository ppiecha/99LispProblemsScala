package fp

import scala.annotation.tailrec

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // prepend operator
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // concatenate two streams

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A]
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException()

  def tail: MyStream[Nothing] = throw new NoSuchElementException()

  def #::[B >: Nothing](element: B): MyStream[B] = new ConsStream[B](element, EmptyStream)

  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()

  def map[B](f: Nothing => B): MyStream[B] = EmptyStream

  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = EmptyStream

  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = EmptyStream

  def take(n: Int): MyStream[Nothing] = EmptyStream

  def takeAsList(n: Int): List[Nothing] = List()
}

class ConsStream[+A](h: A, t: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  override val head: A = h

  override lazy val tail: MyStream[A] = t

  def #::[B >: A](element: B): MyStream[B] = {
    new ConsStream[B](element, new ConsStream[B](head, tail))
  }

  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new ConsStream[B](head, tail ++ anotherStream)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  def map[B](f: A => B): MyStream[B] = new ConsStream[B](f(head), tail.map(f))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new ConsStream[A](head, tail.filter(predicate)) else tail.filter(predicate)

  def take(n: Int): MyStream[A] = {
    if (n > 0) head #:: tail.take(n - 1) else EmptyStream
  }

  def takeAsList(n: Int): List[A] = if (n > 0) head :: tail.takeAsList(n - 1) else List[A]()
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = {
    new ConsStream[A](start, from(generator(start))(generator))
  }
}

object Lazy extends App{
  val test1 = MyStream.from(1)((x: Int) => x + 1)
  println(test1.head, test1.tail.head, test1.tail.tail.head)
  test1.take(10).foreach(println)
  val test0 = MyStream.from(-1)((x: Int) => x - 1)
  println(test1.take(3))
  println(test1.takeAsList(3))
  val test2 = 0 #:: test1
  println(s"test prepend ${test2.takeAsList(5)}")
  val test3 = test0 ++ test1
  println(s"test concatenate ${test3.takeAsList(5)}")
  def test_lazy: Int = {
    println("lazy executed")
    -100
  }
  lazy val t1 = test_lazy
  lazy val t2 = t1
  println(s"test lazy t1 $t1")
  println(s"test lazy t2 $t2")
  println(s"test map ${test1.map(_ * 2).takeAsList(6)}")
  val test4 = test1.flatMap(x => new ConsStream(x, new ConsStream(-x, EmptyStream)))
  println("calculated, printing")
  println(s"test flatmap ${test4.takeAsList(6)}")
  println(test1.filter(_ < 10).take(8).takeAsList(20))

  def lazyFib: MyStream[Int] = {
    def lazyFibAux(a: Int, b: Int): MyStream[Int] = new ConsStream[Int](a, lazyFibAux(b, a + b))
    lazyFibAux(0, 1)
  }

  def lazyFib2: LazyList[Int] = {
    def lazyFibAux(a: Int, b: Int): LazyList[Int] = a #:: lazyFibAux(b, a + b)
    lazyFibAux(0, 1)
  }

  lazyFib.take(10).foreach(println)
  lazyFib2.take(10).foreach(println)

  def primeSieve: LazyList[Int] = {
    def primeSieveAux(list: LazyList[Int]): LazyList[Int] =
      list.head #:: primeSieveAux(list.tail.filterNot(_ % list.head == 0))
    primeSieveAux(LazyList.from(2))
  }
  println("-"*30)
  LazyList.from(2).take(30).foreach(println)
  primeSieve.take(30).foreach(println)

  def primeSieve2: MyStream[Int] = {
    def primeSieveAux(list: MyStream[Int]): MyStream[Int] =
      list.head #:: primeSieveAux(list.tail.filter(_ % list.head != 0))
    primeSieveAux(MyStream.from(1)(_ + 1))
  }

  println("-" * 30)
  primeSieve.take(30).foreach(println)
}

// 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
// 2 => 3 5 7 9 11 13 15 17 19
// 3 => 5 7 11 13 17 19
// 5 => 7 11 13 17 19
// 7 => ...
