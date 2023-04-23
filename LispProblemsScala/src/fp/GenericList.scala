package fp

  sealed abstract class GenericList[+T] {
    def head: T

    def tail: GenericList[T]

    def isEmpty: Boolean

    def add[U >: T](element: U): GenericList[U]

    def toString: String

    def map[B]: (T => B) => GenericList[B]

    def filter: (T => Boolean) => GenericList[T]

    def flatMap[B]: (T => GenericList[B]) => GenericList[B]

    def ++[U >: T]: GenericList[U] => GenericList[U]

    def foreach: (T => Unit) => Unit

    def length: Int

    def reverse: GenericList[T]

    def splitAt: Int => (GenericList[T], GenericList[T])

    def sort: ((T, T) => Int) => GenericList[T]

    def zipWith[B, C]: (GenericList[B], (T, B) => C) => GenericList[C]

    def fold[B >: T](start: B)(op: (B, T) => B): B
  }

  case object Empty extends GenericList[Nothing] {
    def head = throw new NoSuchElementException()

    def tail = throw new NoSuchElementException()

    def isEmpty = true

    def add[U >: Nothing](element: U): GenericList[U] = new Cons(element, Empty)

    override def toString = "Nil"

    override def map[B]: (Nothing => B) => GenericList[B] = _ => Empty

    override def filter: (Nothing => Boolean) => GenericList[Nothing] = _ => Empty

    override def flatMap[B]: (Nothing => GenericList[B]) => GenericList[B] = _ => Empty

    override def ++[U >: Nothing]: GenericList[U] => GenericList[U] = identity

    override def foreach: (Nothing => Unit) => Unit = _ => ()

    override def length: Int = 0

    override def reverse: GenericList[Nothing] = Empty

    override def splitAt: Int => (GenericList[Nothing], GenericList[Nothing]) = _ => (Empty, Empty)

    override def sort: ((Nothing, Nothing) => Int) => GenericList[Nothing] = _ => Empty

    override def zipWith[B, C]: (GenericList[B], (Nothing, B) => C) => GenericList[C] =
      (other, _) => if (other.isEmpty) Empty else throw new Exception("Size doesn't match")

    override def fold[B >: Nothing](start: B)(op: (B, Nothing) => B): B = start
  }

  case class Cons[+T](h: T, t: GenericList[T]) extends GenericList[T] {
    def head: T = h

    def tail: GenericList[T] = t

    def isEmpty = false

    def add[U >: T](element: U): GenericList[U] = new Cons(element, this)

    override def toString: String = "Cons(" + head.toString + ", " + tail.toString + ")"

    override def map[B]: (T => B) => GenericList[B] = transformer => new Cons(transformer(head), tail.map(transformer))

    override def filter: (T => Boolean) => GenericList[T] =
      predicate => if (predicate(head)) new Cons(head, tail.filter(predicate)) else tail.filter(predicate)

    override def flatMap[B]: (T => GenericList[B]) => GenericList[B] =
      transformer => transformer(head) ++ tail.flatMap(transformer)

    override def ++[U >: T]: GenericList[U] => GenericList[U] = x => new Cons(head, tail ++ x)

    override def foreach: (T => Unit) => Unit = f => {f(h); t.foreach(f)}

    override def length: Int = 1 + t.length

    override def reverse: GenericList[T] = {
      def iter: (GenericList[T], GenericList[T]) => GenericList[T] = {
        case (Empty, acc) => acc
        case (Cons(h1, t1), acc) => iter(t1, acc.add(h1))
      }
      iter(this, Empty)
    }

    override def splitAt: Int => (GenericList[T], GenericList[T]) = {
      def iter: (GenericList[T], Int, (GenericList[T], GenericList[T])) => (GenericList[T], GenericList[T]) = {
        case (l, _, (l1, l2)) if l.isEmpty => (l1.reverse, l2.reverse)
        case (Cons(h1, t1), n, (l1, l2)) =>
          //println(n, Cons(h1, t1), l1, l2)
          if (n > 0) iter(t1, n - 1, (l1.add(h1), l2))
          else iter(t1, n - 1, (l1, l2.add(h1)))
        case _ => throw new Exception("Not matched")
      }
      n => iter(this, n, (Empty, Empty))
    }

    override def sort: ((T, T) => Int) => GenericList[T] = {
      def merge: (GenericList[T], GenericList[T], (T, T) => Int) => GenericList[T] = {
        case (Empty, l2, _) => l2
        case (l1, Empty, _) => l1
        case (l1 @ Cons(h1, t1), l2 @ Cons(h2, t2), cmp) =>
          if (cmp(h1, h2) > 0) Cons(h1, merge(t1, l2, cmp))
          else Cons(h2, merge(t2, l1, cmp))
      }
      cmp => {
        splitAt(length / 2) match {
          case (Empty, right) => right
          case (left, Empty) => left
          case (left, right) => merge (left.sort (cmp), right.sort (cmp), cmp)
        }
      }
    }

    override def zipWith[B, C]: (GenericList[B], (T, B) => C) => GenericList[C] = (other, f) =>
      if (other.length != length) throw new Exception("Size doesn't match")
      else Cons(f(h, other.head), t.zipWith(other.tail, f))

    override def fold[B >: T](start: B)(op: (B, T) => B): B = t.fold(op(start, h))(op)
  }

  object GenericList extends App {
    val empty = Empty
    println(empty)
    val l1 = new Cons(3, empty)
    println(l1)
    val l2 = l1.add(2)
    println(l2)
    val l3 = l2.add(1)
    println(l3)
    val res = l3.map(x => x * x)
    println(l3.filter(x => x % 2 == 0))
    println(l3.flatMap(x => new Cons[Int](x, new Cons[Int](x, Empty))))
    println(l3.fold(0)((sum: Int, x: Int) => sum + x * x))
    l3.foreach(println)
    println(l3.sort((x, y) => x - y))
    println(
      l3.zipWith(
        Cons("a", Cons("b", Cons("c", Empty))),
        (x: Int, y: String) => x.toString + y
      )
    )
    println("for works")
    val combinations = for {
      l <- l3
    } yield l
    println(combinations)
  }

