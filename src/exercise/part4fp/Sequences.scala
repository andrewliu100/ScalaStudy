package exercise.part4fp

import scala.util.Random

object Sequences extends App {

  val listOfNumbers: Seq[Int] = Seq(1, 2, 3)
  println(listOfNumbers)
  listOfNumbers.foreach(println)

  val prepended: Seq[Int] = 43 +: listOfNumbers

  val vectors: Seq[Int] = Vector(3, 2, 1)
  println(vectors ++ Vector(4, 5, 6))
  println(vectors.sorted)

  val maxRun = 100
  val maxCap = 1000000

  def getWriteTime(s: Seq[Int]): Double = {
    val rand = Random
    val time = for {
      i <- 1 to maxRun
    } yield {
      val currentTime = System.nanoTime()
      s.updated(rand.nextInt(maxCap), rand.nextInt())
      System.nanoTime() - currentTime
    }
    time.sum * 1.0 / maxRun
  }

  val list = (1 to maxCap).toList
  val vector = (1 to maxCap).toVector
  println(getWriteTime(list))
  println(getWriteTime(vector))
}
