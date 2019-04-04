package exercise.part1

object Functions extends App {
  // 1. A greeting function (name, age) => "Hi, my name is $name and I am $age years old."
  def greeting(name: String, age: Int): String = "Hi, my name is " + name + " and I am " + age + " years old."
  println(greeting("David", 9))

  // 2. Factorial function 1 * 2 * 3 * ... * n
  def factorial(n: Int): Int = if (n == 1) 1 else n * factorial(n - 1)
  println(factorial(5))

  // 3. A Fibonacci function
  // f(1) = 1, f(2) = 1, ..., f(n) = f(n-1) + f(n-2)
  def fibonacci(n: Int): Int = if (n == 1 || n == 2) 1 else fibonacci(n-1) + fibonacci(n-2)
  println(fibonacci(6)) // 1 1 2 3 5 8

  // 4. Test if a number is prime. 15: 2,3
  def isPrime(n: Int): Boolean = {
    val sqrt: Int = Math.sqrt(n).intValue()
    def isPrimeUtil(m: Int): Boolean = if (m <= 1) true else n % m != 0 && isPrimeUtil(m-1)
    isPrimeUtil(sqrt)
  }
  println(isPrime(37))
  println(isPrime(13 * 17))
}
