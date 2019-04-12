package exercise.part4fp

object Functions extends App {

  // 1. a function which takes 2 string and concatenates them
  def concatenator: ((String, String) => String) = new Function2[String, String, String] {
    override def apply(s1: String, s2: String): String = s1 + s2
  }
  println(concatenator("Hello ", "Scala"))

  // 2. transform the MyPredicate and MyTransformer to function types
  abstract class MyList[+E] {
    def head: E
    def tail: MyList[E]
    def isEmpty: Boolean
    def add[A >: E](e: A): MyList[A] // A is a super class of E
    def map[B](transformer: E => B): MyList[B]
    def flatMap[B](transformer: E => MyList[B]): MyList[B]
    def filter(predicate: E => Boolean): MyList[E]
    def printElement: String

    override def toString: String = "[" + printElement + "]"
  }

  object Empty extends MyList[Nothing] {
    override def head: Nothing = throw new NoSuchElementException

    override def tail: MyList[Nothing] = throw new NoSuchElementException

    override def isEmpty = true

    override def add[A >: Nothing](e: A): MyList[A] = new Cons(e, Empty)

    override def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty

    override def map[B](transformer: Nothing => B): MyList[B] = Empty

    override def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty

    override def printElement: String = ""
  }

  class Cons[+E](h: E, t:MyList[E]) extends MyList[E] {
    override def head: E = h

    override def tail: MyList[E] = t

    override def isEmpty = false

    override def add[A >: E](e: A): MyList[A] = new Cons(e, this)

    override def filter(predicate: E => Boolean): MyList[E] = {
      if (predicate(h)) new Cons(h, t.filter(predicate))
      else t.filter(predicate)
    }

    override def map[B](transformer: E => B): MyList[B] =
      new Cons(transformer(h), t.map(transformer))

    override def flatMap[B](transformer: E => MyList[B]): MyList[B] = {
      ++(transformer(head), tail.flatMap(transformer))
    }

    def ++[B](a: MyList[B], b: MyList[B]): MyList[B] = {
      if (a == Empty) b
      else new Cons(a.head, ++(a.tail, b))
    }

    override def printElement: String = {
      if(t.isEmpty) "" + h
      else h + " " + t.printElement
    }
  }

  val listOfInteger: MyList[Int] = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, Empty)))
  val listOfString: MyList[String] = new Cons[String]("Hello", new Cons[String]("Scala", Empty))

  println(listOfInteger.toString)
  println(listOfString.toString)

  println(listOfInteger.filter(new Function1[Int, Boolean] {
    override def apply(e: Int): Boolean = e % 2 == 0
  }))
  println(listOfInteger.map(new Function1[Int, Int] {
    override def apply(a: Int): Int = a * 2
  }))
  println(listOfInteger.flatMap(new Function1[Int, MyList[Int]] {
    override def apply(a: Int) = new Cons[Int](a, new Cons(a + 1, Empty))
  }))

  // 3. define a function which takes an int and returns another function which takes an int and returns an int

  val superAdder: Function1[Int, Function1[Int, Int]] = new Function1[Int, Function1[Int, Int]] {
    override def apply(a: Int): Function1[Int, Int] = new Function1[Int, Int] {
      override def apply(b: Int): Int = a + b
    }
  }

  println(superAdder(3)(4))

}
