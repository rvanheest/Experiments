package experiments.pickling

trait example3 {

	case class Player(firstName: String,
										lastName: String,
										position: String,
										atBats: Option[Int],
										hits: Option[Int],
										era: Option[Int])
	case class Team(name: String, city: String, players: Seq[Player])
	type Divisions = Map[String, Team]
	type Leagues = Map[String, Divisions]
	case class Season(year: Int, leagues: Leagues)



}
