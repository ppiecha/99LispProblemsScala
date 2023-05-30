package fp

trait MySet[A] extends (A => Boolean) {
  def apply(elem: A): Boolean = contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  def remove(elem: A): MySet[A]
  def intersection(anotherSet: MySet[A]): MySet[A]
  def difference(anotherSet: MySet[A]): MySet[A]

  //def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false

  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new EmptySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  def filter(predicate: A => Boolean): MySet[A] = this

  def foreach(f: A => Unit): Unit = ()

  def remove(elem: A): MySet[A] = this

  def intersection(anotherSet: MySet[A]): MySet[A] = this

  def difference(anotherSet: MySet[A]): MySet[A] = this

  //def unary_! : MySet[A] =  => true
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {

  def contains(elem: A): Boolean = (elem == head) || tail.contains(elem)

  def +(elem: A): MySet[A] =
    if (this contains elem) this
    else new NonEmptySet(elem, this)

  def ++(anotherSet: MySet[A]): MySet[A] = (anotherSet + head) ++ tail

  def map[B](f: A => B): MySet[B] = new NonEmptySet(f(head), tail.map(f))

  def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)

  def filter(predicate: A => Boolean): MySet[A] =
    if (predicate(head)) new NonEmptySet[A](head, tail.filter(predicate))
    else tail.filter(predicate)

  def foreach(f: A => Unit): Unit = this.map(f)

  def remove(elem: A): MySet[A] = if (head == elem) tail else tail.remove(elem) + head

  def intersection(anotherSet: MySet[A]): MySet[A] = {
    filter(anotherSet)
//    val new_tail = tail.intersection(anotherSet)
//    if (anotherSet contains head) new_tail + head
//    else new_tail
  }

  def difference(anotherSet: MySet[A]): MySet[A] = ???
//    filter(!anotherSet)
}

object TestMySet extends App {

  def test(values: Int*): Seq[Int] = {
    println(values.getClass)
    values.toSeq
  }

  val set = new NonEmptySet(1, new EmptySet)
  set.foreach(println)

  println(test(3,4,5))
}

