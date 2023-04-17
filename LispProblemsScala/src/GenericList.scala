package Generic {

  abstract class GenericList[T] {
    def head: T

    def tail: GenericList[T]

    def isEmpty: Boolean

    def add(element: T): GenericList[T]

    def toString: String
  }

  class Empty[T] extends GenericList[T] {
    def head = throw new NoSuchElementException()

    def tail = throw new NoSuchElementException()

    def isEmpty = true

    def add(element: T): GenericList[T] = new Cons(element, new Empty[T])

    override def toString = "Nil"
  }

  class Cons[T](h: T, t: GenericList[T]) extends GenericList[T] {
    def head = h

    def tail = t

    def isEmpty = false

    def add(element: T) = new Cons(element, this)

    override def toString: String = "Cons(" + head.toString + ", " + tail.toString + ")"
  }

  object GenericList extends App {
    val empty = new Empty[String]
    println(empty)
    val l1 = new Cons("part1", empty)
    println(l1)
    val l2 = l1.add("part0")
    println(l2)
  }

}

