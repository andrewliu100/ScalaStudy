package exercise.part2oop

object Generics extends App {

  trait MyPredicate[-T] {
    def test(e: T): Boolean
  }

  trait MyTransformer[-A, B] {
    def transform(a: A): B
  }

  // Covariant
  abstract class MyList[+E] {
    def head: E
    def tail: MyList[E]
    def isEmpty: Boolean
    def add[A >: E](e: A): MyList[A] // A is a super class of E
    def map[B](transformer: MyTransformer[E, B]): MyList[B]
    def flatMap[B](transformer: MyTransformer[E, MyList[B]]): MyList[B]
    def filter(predicate: MyPredicate[E]): MyList[E]
    def printElement: String

    override def toString: String = "[" + printElement + "]"
  }

  object Empty extends MyList[Nothing] {
    override def head: Nothing = throw new NoSuchElementException

    override def tail: MyList[Nothing] = throw new NoSuchElementException

    override def isEmpty = true

    override def add[A >: Nothing](e: A): MyList[A] = new Cons(e, Empty)

    override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty

    override def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty

    override def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty

    override def printElement: String = ""
  }

  class Cons[+E](h: E, t:MyList[E]) extends MyList[E] {
    override def head: E = h

    override def tail: MyList[E] = t

    override def isEmpty = false

    override def add[A >: E](e: A): MyList[A] = new Cons(e, this)

    override def filter(predicate: MyPredicate[E]): MyList[E] = {
      if (predicate.test(h)) new Cons(h, t.filter(predicate))
      else t.filter(predicate)
    }

    override def map[B](transformer: MyTransformer[E, B]): MyList[B] =
      new Cons(transformer.transform(h), t.map(transformer))

    override def flatMap[B](transformer: MyTransformer[E, MyList[B]]): MyList[B] = {
      ++(transformer.transform(head), tail.flatMap(transformer))
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

  println(listOfInteger.filter(new MyPredicate[Int] {
    override def test(e: Int): Boolean = e % 2 == 0
  }))
  println(listOfInteger.map(new MyTransformer[Int, Int] {
    override def transform(a: Int): Int = a * 2
  }))
  println(listOfInteger.flatMap(new MyTransformer[Int, MyList[Int]] {
    override def transform(a: Int) = new Cons[Int](a, new Cons(a + 1, Empty))
  }))
}
