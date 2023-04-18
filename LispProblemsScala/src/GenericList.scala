package Generic {

  abstract class GenericList[+T] {
    def head: T

    def tail: GenericList[T]

    def isEmpty: Boolean

    def add[U >: T](element: U): GenericList[U]

    def toString: String
  }

  object Empty extends GenericList[Nothing] {
    def head = throw new NoSuchElementException()

    def tail = throw new NoSuchElementException()

    def isEmpty = true

    def add[U >: Nothing](element: U): GenericList[U] = new Cons(element, Empty)

    override def toString = "Nil"
  }

  class Cons[+T](h: T, t: GenericList[T]) extends GenericList[T] {
    def head = h

    def tail = t

    def isEmpty = false

    def add[U >: T](element: U): GenericList[U] = new Cons(element, this)

    override def toString: String = "Cons(" + head.toString + ", " + tail.toString + ")"
  }

  object GenericList extends App {
    val empty = Empty
    println(empty)
    val l1 = new Cons("part1", empty)
    println(l1)
    val l2 = l1.add("part0")
    println(l2)
  }

}

