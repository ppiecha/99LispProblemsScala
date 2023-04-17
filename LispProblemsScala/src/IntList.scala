abstract class IntList {
  def head: Int

  def tail: IntList

  def isEmpty: Boolean

  def add(element: Int): IntList

  def toString: String
}

object Empty extends IntList {
  def head = throw new UnsupportedOperationException()
  def tail = throw new UnsupportedOperationException()
  def isEmpty = true
  def add(element: Int): IntList = new Cons(element, Empty)
  override def toString = "Nil"
}

class Cons(h: Int, t: IntList) extends IntList {
  def head = h

  def tail = t

  def isEmpty = false

  def add(element: Int): IntList = new Cons(element, this)

  override def toString: String = "Cons(" + head.toString + ", " + tail.toString + ")"
}

object IntList extends App {
  val empty = Empty
  println(empty)
  val l1 = new Cons(1, empty)
  println(l1)
  val l2 = l1.add(0)
  println(l2)
}
