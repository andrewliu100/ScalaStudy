package exercise.part4fp

object MapFlatMapFilter extends App {

    val nums = List(1, 2, 3)
    val strs = List("Hello", "Scala")
    val chars = List('a', 'b', 'c', 'd')

    val combinations = nums.filter(_ % 2 !=0).flatMap(i => chars.flatMap(c => strs.map(s => "" + c + i + "-" + s)))
    println(combinations)

    // for comprehensions
    val combs = for {
      i <- nums if i % 2 == 0
      c <- chars
      s <- strs
    } yield "" + c + i + "-" + s
    println(combs)

    /*
    Exercise
     */
    abstract class Maybe[+T] {

      def map[B](f: T => B): Maybe[B]

      def flatMap[B](f: T => Maybe[B]): Maybe[B]

      def filter(p: T => Boolean): Maybe[T]
    }

    case object MaybeNot extends Maybe[Nothing] {
      override def map[B](f: Nothing => B): Maybe[B] = MaybeNot

      override def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = MaybeNot

      override def filter(p: Nothing => Boolean): Maybe[Nothing] = MaybeNot
    }

    case class Just[+T](value: T) extends Maybe[T] {
      override def map[B](f: T => B): Maybe[B] = Just(f(value))

      override def flatMap[B](f: T => Maybe[B]): Maybe[B] = f(value)

      override def filter(p: T => Boolean): Maybe[T] =
        if (p(value)) this
        else MaybeNot
    }

    val just3 = Just(3)
    println(just3)
    println(just3.map(_ * 2))
    println(just3.flatMap(x => Just(x % 2 == 0)))
    println(just3.filter(_ % 2 ==0))

}
