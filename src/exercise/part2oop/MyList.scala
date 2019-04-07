package exercise.part2oop

abstract class MyList {
  def head: Int
  def tail: MyList
  def isEmpty: Boolean
  def add(e: Int): MyList
  def printElement: String

  override def toString: String = "[" + printElement + "]"
}

object Empty extends MyList {
  override def head = throw new NoSuchElementException

  override def tail = throw new NoSuchElementException

  override def isEmpty = true

  override def add(e: Int) = new Cons(e, Empty)

  override def printElement: String = ""
}

class Cons(h: Int, t:MyList) extends MyList {
  override def head = h

  override def tail = t

  override def isEmpty = false

  override def add(e: Int) = new Cons(e, this)

  override def printElement: String = {
    if(t.isEmpty) "" + h
    else h + " " + t.printElement
  }
}

object ListTest extends App {
  val list = new Cons(1, new Cons(2, new Cons(3, Empty)))
  println(list.head)
  println(list.add(4).toString)
  println(list.toString)
}
