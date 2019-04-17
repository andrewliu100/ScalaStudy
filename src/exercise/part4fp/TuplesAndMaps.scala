package exercise.part4fp

object TuplesAndMaps extends App {

  val phonebook = Map("Jim" -> 555, "JIM" -> 900)
  println(phonebook.map(pair => pair._1.toLowerCase -> pair._2))

  /* Overly simplified social network with maps
      Person = String
       - add person to the network
       - remove
       - friend (mutual)
       - unfriend

       - number of friends of a person
       - person with most friends
       - how many people have NO friends
       - if there is a social connection between two people
  */

  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = network + (person -> Set())

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    def removeAux(friends: Set[String], networkAcc: Map[String, Set[String]]): Map[String, Set[String]] = {
      if (friends.isEmpty) networkAcc
      else removeAux(friends.tail, unfriend(network, friends.head, person))
    }
    val unfriends = removeAux(network(person), network)
    unfriends - person
  }

  def friend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)
    network + (a -> (friendsA + b)) + (b -> (friendsB + a))
  }

  def unfriend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)
    network + (a -> (friendsA - b)) + (b -> (friendsB - a))
  }

  def numberOfFriends(network: Map[String, Set[String]], a: String): Int = network(a).size

  def personWithMostFriends(network: Map[String, Set[String]]): String =
    network.maxBy(pair => pair._2.size)._1

  def nOfNoFriends(network: Map[String, Set[String]]): Int =
    network.count(pair => pair._2.isEmpty)

  def isConnected(network: Map[String, Set[String]], a: String, b: String): Boolean = {
    def dfs() = ???
    false
  }

  val people: Map[String, Set[String]] =
    add(add(add(add(Map(),"Mary"), "John"), "Bob"), "Dan")
  val aNetwork = friend(friend(people, "Mary", "John"), "Mary", "Bob")
  println(aNetwork)
  println(unfriend(aNetwork, "John", "Mary"))
  println(remove(aNetwork, "John"))
  println(numberOfFriends(aNetwork, "Mary"))
  println(personWithMostFriends(aNetwork))
  println(nOfNoFriends(aNetwork))
  println(isConnected(aNetwork, "Bob", "John"))
}
