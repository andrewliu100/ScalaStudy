package exercise.part4fp

import scala.util.{Failure, Random, Success, Try}

object HandlingFailure extends App {

  val success = Success(3)
  val failure = Failure(new RuntimeException("Failure"))
  println(success)
  println(failure)

  def unsafeMethod(): String = throw new RuntimeException("No String")

  val s = Try(unsafeMethod())
  println(s.getOrElse("Default String"))
  println(s.isSuccess)
  println(Try("some value").getOrElse("other value"))

  val fallbackVal = Try(unsafeMethod()).orElse(Try("default"))
  println(fallbackVal)

  // Better
  def betterUnsafeMethod(): Try[String] = Failure(new RuntimeException("Fail"))
  def betterBackupMethod(): Try[String] = Success("Valid")

  val betterFallback = betterBackupMethod() orElse betterBackupMethod()
  println(betterFallback)

  // map, flatMap, filter
  println(success.map(_ * 2))
  println(success.flatMap(x => Success(x * 10)))
  println(success.filter(_ > 10))


  // Exercise
  val hostname = "localhost"
  val port ="8080"

  def renderHtml(html: String) = println(html)

  class Connection {
    val rand = Random
    def get(url: String): String =
      if (rand.nextBoolean()) "<html>"
      else throw new RuntimeException("Connection interrupted")
  }

  object HttpService {
    val rand = Random
    def getConnection(host: String, port: String): Connection =
      if (rand.nextBoolean()) new Connection
      else throw new RuntimeException("No connection")
  }

  val html = Try(HttpService.getConnection(hostname, port)).flatMap(conn => Try(conn.get("url")))
  println(html.isSuccess)
  html.foreach(renderHtml)

  // try comprehensions
  for {
    conn <- Try(HttpService.getConnection(hostname, port))
    result <- Try(conn.get("url"))
  } renderHtml(result)
}
