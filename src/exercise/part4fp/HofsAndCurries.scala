package exercise.part4fp

object HofsAndCurries extends App {

  abstract class MyList[+E] {
    def head: E
    def tail: MyList[E]
    def isEmpty: Boolean
    def add[A >: E](e: A): MyList[A] // A is a super class of E
    def map[B](transformer: E => B): MyList[B]
    def flatMap[B](transformer: E => MyList[B]): MyList[B]
    def filter(predicate: E => Boolean): MyList[E]
    def printElement: String

    // 1. HOFs: Expand MyList
    //  - foreach method A => Unit
    //    [1,2,3].foreach(x=>println(x))
    def foreach(f: E => Unit): Unit
    //  - sort function ((A, A) => Int) => MyList
    //    [1,2,3].sort((x, y) => y - x) => [3,2,1]
    def sort(compare: (E, E) => Int): MyList[E]
    //  - zipWith (list, (A,A) => B) => MyList[B]
    //    [1,2,3].zipWith([4,5,6], x * y) => [1*4, 2*5, 3*6]
    def zipWith[B, C](list: MyList[B], f: (E, B) => C): MyList[C]
    //  - fold(start)(function) => a value
    //    [1,2,3].fold(0)(x+y) = 6
    def fold[B](start: B)(operator: (B, E) => B): B

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

    // hofs
    def foreach(f: Nothing => Unit): Unit = ()

    def sort(compare: (Nothing, Nothing) => Int): MyList[Nothing] = Empty

    def zipWith[B, C](list: MyList[B], zip: (Nothing, B) => C): MyList[C] =
      if (!list.isEmpty) throw new RuntimeException("Lists do not have the same length")
      else Empty

    def fold[B](start: B)(f: (B, Nothing) => B): B = start

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

    // hofs
    override def foreach(f: E => Unit): Unit = {
      f(h)
      t.foreach(f)
    }

    override def sort(compare: (E, E) => Int): MyList[E] = {
      def insert(x: E, sortedList: MyList[E]): MyList[E] =
        if (sortedList.isEmpty) new Cons(x, Empty)
        else if (compare(x, sortedList.head) <= 0) new Cons(x, sortedList)
        else new Cons(sortedList.head, insert(x, sortedList.tail))

      val sortedTail = t.sort(compare)
      insert(h, sortedTail)
    }

    override def zipWith[B, C](list: MyList[B], zip: (E, B) => C): MyList[C] =
      if (list.isEmpty) throw new RuntimeException("Lists do not have the same length")
      else new Cons(zip(h, list.head), t.zipWith(list.tail, zip))

    override def fold[B](start: B)(operator: (B, E) => B): B = t.fold(operator(start, h))(operator)
  }

  val listOfInteger: MyList[Int] = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, Empty)))
  val anotherListOfInteger: MyList[Int] = new Cons[Int](4, new Cons[Int](5, Empty))
  val listOfString: MyList[String] = new Cons[String]("Hello", new Cons[String]("Scala", Empty))

  listOfInteger.foreach(println)
  println(listOfInteger.sort((x, y) => y - x))
  println(anotherListOfInteger.zipWith(listOfString, (i: Int, s: String) => i + "-" + s))
  println(listOfInteger.fold(0)(_ + _))

  // 2. toCurry(f: (Int, Int) => Int) => (Int => Int => Int)
  //    fromCurry(f: Int => Int => Int) => (Int, Int) => Int

  // (x, y) => x + y = x => y => x + y

  def toCurry(f: (Int, Int) => Int): Int => Int => Int = x => y => f(x, y)
  def fromCurry(f: Int => Int => Int): (Int, Int) => Int = (x, y) => f(x)(y)

  // 3. compose(f,g) => x => f(g(x))
  //    andThen(f,g) => x => g(f(x))

  def compose[A,B,T](f: A => B, g: T => A): T=>B = x => f(g(x))
  def andThen[A,B,T](f: B => A, g: A => T): B=>T = x => g(f(x))

  val add2 = (x: Int) => x + 2
  val multi3 = (x: Int) => x * 3
  println(compose(add2, multi3)(2))
  println(andThen(add2, multi3)(2))
}


