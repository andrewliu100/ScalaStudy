package exercise.part2oop


object OOP extends App {

  val writer = new Writer("Andrew", "Liu", 1990)
  val novel = new Novel("novel1", 2009, writer)

  println(writer.fullname)

  val counter = new Counter(0)
  counter.increment(10).print()
}
/*
  Novel and a Writer

  Writer: first name, surname, year of birth
    - method fullname

  Novel: name, year of release, author
    - method authorAge
    - isWrittenBy(author)
    - copy(new year of release) = new instance of Novel

  */
class Writer(firstName: String, surname: String, val yearOfBirth: Int) {
  def fullname: String = s"$firstName $surname"
}

class Novel(name: String, yearOfRelease: Int, author: Writer) {

  def authorAge: Int = author.yearOfBirth - yearOfRelease
  def isWrittenBy(author: Writer) = author == this.author
  def copy(newReleaseYear: Int): Novel = new Novel(name, newReleaseYear, author)
}

/*
  Counter class
    - receives an int value
    - method current count
    - method to increment/decrement => new Counter
    - overload inc/dec to receive an amount
 */

class Counter(val value: Int) {
  def current: Int = this.value
  def increment: Counter = new Counter(this.value + 1)
  def decrement: Counter = new Counter(this.value - 1)

  def increment(n: Int): Counter = {
    if(n <= 0) this
    else increment.increment(n - 1)
  }

  def decrement(n: Int): Counter = {
    if (n <= 0) this
    else decrement.decrement(n - 1)
  }

  def print(): Unit = {
    println(this.value)
  }
}
