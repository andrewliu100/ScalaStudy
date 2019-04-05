package exercise.part1

import scala.annotation.tailrec

object Recursions extends App {

  // 1. Concatenate a String n times
  def concat(s: String, n: Int): String = if(n <= 1) s else s + concat(s, n - 1)
  println(concat("s", 9))


  def concatTailRec(s: String, n: Int): String = {
    @tailrec
    def concateHelper(n: Int, accumulator: String): String =
      if(n <= 1) accumulator
      else concateHelper(n - 1, s + accumulator)
    concateHelper(n, s)
  }
  println(concatTailRec("s", 20000))

  // 2. Factorial function 1 * 2 * 3 * ... * n
  def factorial(n: Int): Int = if (n == 1) 1 else n * factorial(n - 1)
  println(factorial(5))


  def factorialTailRec(n: Int): BigInt = {
    @tailrec
    def factHelper(x: Int, accumulator: BigInt): BigInt =
      if(x <= 1) accumulator
      else factHelper(x - 1, x * accumulator)
    factHelper(n, 1)
  }
  println(factorialTailRec(20000))


  // 3. A Fibonacci function
  // f(1) = 1, f(2) = 1, ..., f(n) = f(n-1) + f(n-2)
  def fibonacci(n: Int): Int = if (n == 1 || n == 2) 1 else fibonacci(n-1) + fibonacci(n-2)
  println(fibonacci(6)) // 1 1 2 3 5 8


  def fibTailRec(n: Int): BigInt = {
    @tailrec
    def fibHelper(x: Int, fx1: BigInt, fx2: BigInt): BigInt =
      if(x <= 2) fx2
      else fibHelper(x - 1, fx2, fx1 + fx2)
    fibHelper(n, 1, 1)
  }
  println(fibTailRec(10000))



  // 4. Test if a number is prime. 15: 2,3
  def isPrime(n: Int): Boolean = {
    val sqrt: Int = Math.sqrt(n).intValue()
    def isPrimeUtil(m: Int): Boolean = if (m <= 1) true else n % m != 0 && isPrimeUtil(m-1)
    isPrimeUtil(sqrt)
  }
  println(isPrime(37))
  println(isPrime(13 * 17))

  def isPrimeTailRec(n: Int): Boolean = {
    @tailrec
    def isPrimeHelper(x: Int, isPrimeUtilX: Boolean): Boolean =
      if(x <= 1) isPrimeUtilX
      else isPrimeHelper(x - 1, isPrimeUtilX && n % x != 0)
    val sqrt: Int = Math.sqrt(n).intValue()
    isPrimeHelper(sqrt, isPrimeUtilX = true)
  }
  println(isPrime(10000003))
}
