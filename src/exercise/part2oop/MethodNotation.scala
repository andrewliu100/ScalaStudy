package exercise.part2oop

object MethodNotation extends App {

  /*
  1. Overload the + operator
    mary + "the rockstar" => new person "mary (the rockstar)"
  2. Add an age to the Person class
     Add a unary + operator => new person with the age + 1
     +mary => mary with the age increment
  3. Add a "learns" method => "mary learns scala"
     Add a learnsScala method, calls learns method with "scala"
     Use it in postfix notation
  4. Overload the apply method
     mary.apply(2) => "Mary watched Inceptions 2 times"
   */
  class Person(val name: String, val favoriteMovie: String, val age: Int = 0) {
    def +(person: Person) : String = s"${this.name} is hanging out with ${person.name}"
    // 1
    def +(nickname: String): Person = new Person(s"${this.name} ($nickname)", favoriteMovie)
    // 2
    def unary_+ : Person = new Person(name, favoriteMovie, age + 1)
    // 3
    def learns(course: String): String = s"${this.name} learns $course"
    def learnsScala: String = learns("scala")
    // 4
    def apply(n: Int): String = s"${this.name} watched ${favoriteMovie} $n times"

  }

  val mary = new Person("Mary", "Inception")
  println((mary + "the rockstar")(2))
  println((+mary).age)
  println(mary learnsScala)
  println(mary(10))
}
