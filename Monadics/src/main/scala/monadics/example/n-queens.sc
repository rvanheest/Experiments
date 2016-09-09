def queensOption(n: Int): Option[List[Int]] = {

	def safe(q: Int, qs: List[Int]): Boolean = !qs.contains(q)

	n match {
		case 0 => Option(Nil)
		case x =>
			for {
				qs <- queensOption(x - 1)
				q <- (1 to 8).map(Option(_)).reduce(_.orElse(_))
				if safe(q, qs)
			} yield q :: qs
	}
}

def queensList(n: Int): List[List[Int]] = {

	def safe(q: Int, qs: List[Int]): Boolean = !qs.contains(q)

	n match {
		case 0 => List(Nil)
		case x =>
			for {
				qs <- queensList(x - 1)
				q <- (1 to 8).map(List(_)).reduce(_ ++ _)
				if safe(q, qs)
			} yield q :: qs
	}
}

queensOption(8)
queensList(8).contains(List(4, 2, 7, 3, 6, 8, 5, 1))
