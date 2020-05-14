package experiments.dependencyInjection.restaurant

object ManualDI extends App {

	case class Field()
	case class Digger()
	case class PotatoFarm(field: Field, digger: Digger)

	case class CowPasture(potatoFarm: PotatoFarm)
	case class Meatery(cowPasture: CowPasture)

	case class Restaurant(potatoFarm: PotatoFarm, meatery: Meatery) {
		def orderSteakWithPotatoes(): Unit = {
			println(s"Welcome to $this. Here's your order!")
		}
	}

	// manual dependency injection: use lazy vals to let the runtime determine when to instantiate the dependencies
	lazy val potatoFarm = PotatoFarm(field, digger)
	lazy val field = Field()
	lazy val digger = Digger()

	lazy val cowPasture = CowPasture(potatoFarm)
	lazy val meatery = Meatery(cowPasture)

	lazy val restaurant = Restaurant(potatoFarm, meatery)
	restaurant.orderSteakWithPotatoes()
}
