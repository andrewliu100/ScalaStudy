package exercise.part4fp

import scala.util.Random

object Options extends App {

  val someValue = Some(4)
  val noneValue = None

  println(someValue.map(_ * 2))
  println(someValue.filter(_ > 10))
  println(someValue.flatMap(x => Option(x * 10)))


  // Exercise

  val config: Map[String, String] = Map("Port" -> "80")

  class Connection {
    def connect = "Connected"
  }

  object Connection {
    def apply(host: String, port: String): Option[Connection] = {
      val rand: Random = Random
      if (rand.nextBoolean()) Some(new Connection)
      else None
    }
  }

  // try to establish a connection, if so - print connect method
  val host = config.get("Host")
  println(host)
  val port = config.get("Port")
  val connection = host.flatMap(h => port.flatMap(p => Connection(h, p)))
  println(connection)
  val status =connection.map(c => c.connect)
  println(status)
  status.foreach(println)

  // for comprehensions
  val connStatus = for {
    host <- config.get("Host")
    port <- config.get("Port")
    connection <- Connection(host, port)
  } yield connection.connect
}
