package fp

import scala.annotation.tailrec
import scala.util.{Random, Try}

object Maps extends App{

  val test1 = Map("Jim" -> 555, "JIM" -> 666)
  println(test1)

  type P = Map[String, List[String]]

  val network: P = Map().withDefaultValue(List[String]())

  def add(net: P, p: String): P =
    net + (p -> List())

  val n1 = add(network, "Piotr")
  println(n1)

  def remove(net: P, p: String): P =
    net - p

  println(remove(n1, "Piotr"))

  val test2 = Map("Ania" -> "Piotr")
  println(test2 - "ppp")

  def addFriend(net: P, p: String, f: String): P = {
    //println(net(p), f :: net(p))
    val network = net.updated(p, f :: net(p))
    network.updated(f, p :: network(f))
  }

  val n2 = addFriend(n1, "Piotr", "Ania")
  println(n2)

  def unFriend(net: P, p: String, f: String): P = {
    val network = net.updated(p, net(p).filter(_ != f))
    network.updated(f, network(f).filter(_ != p))
  }

  println(unFriend(n2, "Piotr", "Ania"))

  def friendsNumber(net: P, p: String): Int =
    net(p).length

  println(friendsNumber(n1, "Piotr"))
  println(friendsNumber(n2, "Piotr"))

  val n3 = addFriend(n2, "Piotr", "Kamil")
  println(n3)

  def mostFriends(net: P): (String, List[String]) = {
    val (name, count) = net.mapValues(_.length).toList.sortBy(pair => pair._2)(Ordering[Int].reverse).head
    println(name, count)
    (name, net(name))
  }

  println(mostFriends(n3))

  def noFriendsCount(net: P): Int =
    net.count(_._2.isEmpty)

  val n4 = add(n3, "John")
  println(n4)

  println(noFriendsCount(n4))
  println(noFriendsCount(n3))

  def isConnection(net: P, p1: String, p2: String): Boolean = {
    @tailrec
    def hasConnection(connections: List[String], p2: String): Boolean = connections match {
      case List() => false
      case l if l.contains(p2) => true
      case x :: xs => if (net(x) contains p2) true else hasConnection(xs, p2)
    }
    hasConnection(net(p1), p2)
  }

  println(isConnection(n4, "Kamil", "Ania"))
  println(isConnection(n4, "Kamil", "John"))

  val config: Map[String, String] = Map(
    "host" -> "10.1.1.2",
    "port" -> "80"
  )

  class Connection {
    def connect = "Connected" // connect to some server
  }

  object Connection {
    val random = new Random(System.nanoTime())

    def apply(host: String, port: String): scala.Option[Connection] =
      if (random.nextBoolean()) scala.Some(new Connection)
      else scala.None
  }

  //try to establish connection - if so print the connect method

  val con = Connection(host = config("host"), port = config("port"))
  val test: scala.Option[Int] = scala.Some(1)
  if (test.isEmpty) println("vvv")

  if (con.isDefined) println(con.map(_.connect))
  else println("not connected")

  val forConnectionStatus = for {
    host <- config.get("host")
    port <- config.get("port")
    connection <- Connection(host, port)
  } yield connection.connect

  forConnectionStatus.foreach(println)

  def unSafe = throw new RuntimeException("yaicks")

  val resp = scala.util.Try(unSafe)
  println(resp)

  val parseRange: PartialFunction[Any, Int] = {
    case x: Int if x > 10 => x + 1
  }
  println(List(15, 3, "aString") collect {
    parseRange
  })

//  println(List(15, 3, "aString") map { parseRange })

  println(List(1, 2) collect { case i: Int => i > 10 })
  println(List(1, 2) filter { i: Int => i > 10 })

  val hostname = "localhost"
  val port = "8080"
  def renderHTML(page: String) = println(page)

  class Connection2 {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html>...</html>"
      else throw new RuntimeException("Connection interrupted")
    }
  }

  object HttpService {
    val random = new Random(System.nanoTime())

    def getConnection(host: String, port: String): Connection2 =
      if (random.nextBoolean()) new Connection2
      else throw new RuntimeException("wrong port")
  }

  for {
    connection <- Try(HttpService.getConnection(hostname, port))
    page <- Try(connection.get("myUrl"))
  } yield renderHTML(page)
}
