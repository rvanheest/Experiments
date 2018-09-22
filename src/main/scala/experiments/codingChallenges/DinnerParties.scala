package experiments.codingChallenges

// find every party to fill a table from a list of all friends
object DinnerParties extends App {

  val friends = List("a", "b", "c", "d", "e")
  val size = 3

  for (group <- findDinnerParties(friends, size))
    println(group)

  def findDinnerParties(friends: List[String], tableSize: Int): List[List[String]] = {
    val groups = List.newBuilder[List[String]]

    def combineFriends(localFriends: List[String], localTableSize: Int, group: List[String] = Nil): Unit = {
      if (localTableSize <= 0)
        groups += group.reverse
      else
        localFriends match {
          case Nil => // throw new IllegalArgumentException(s"Too few friends to create a group. Currently you have $group while having $tableSize seats ($localTableSize seats left)")
          case f :: fs =>

            // leave
            combineFriends(fs, localTableSize, group)

            // take
            combineFriends(fs, localTableSize - 1, f :: group)
        }
    }

    combineFriends(friends, tableSize)
    groups.result().reverse
  }
}
