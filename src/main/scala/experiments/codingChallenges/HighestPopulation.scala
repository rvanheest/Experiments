package experiments.codingChallenges

object HighestPopulation extends App {

  // Given a list of birth- and death years,
  // calculate in which year was the population the highest?

  type BirthYear = Int
  type DeathYear = Int

  val example1 = Seq(2000 -> 2001, 2001 -> 2002)
  println(yearWithHighestPopulation(example1))

  def yearWithHighestPopulation(years: Seq[(BirthYear, DeathYear)]): Int = {
    years.flatMap { case (bYear, dYear) => Seq(bYear -> true, dYear + 1 -> false) }
      .sortBy { case (year, _) => year }
      .foldLeft((0, 0, 0)) {
        case ((count, maxCount, year), (cYear, true)) =>
          val nextCount = count + 1
          if (nextCount > maxCount)
            (nextCount, nextCount, cYear)
          else
            (nextCount, maxCount, year)
        case ((count, maxCount, year), (_, false)) => (count - 1, maxCount, year)
      }
      ._3
  }
}
