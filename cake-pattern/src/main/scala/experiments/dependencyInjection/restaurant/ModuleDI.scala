package experiments.dependencyInjection.restaurant

object ModuleDI extends App {

	trait AgriculturalComponent {
		val potatoFarm: PotatoFarm

		trait PotatoFarm
	}

	trait TraditionalAgriculturalComponent extends AgriculturalComponent {
		val field: Field
		val digger: Digger

		case class Field()
		case class Digger()
		case class TraditionalPotatoFarm(field: Field, digger: Digger) extends PotatoFarm {
			println("Traditional potato farm! Rejoice!")
		}
	}

	trait ModernAgriculturalComponent extends AgriculturalComponent {
		case class ModernPotatoFarm() extends PotatoFarm {
			println("Modern potato farm")
		}
	}

	trait LivestockComponent {
		this: AgriculturalComponent =>

		val cowPasture: CowPasture
		val meatery: Meatery

		case class CowPasture(potatoFarm: PotatoFarm)
		case class Meatery(cowPasture: CowPasture)
	}

	trait RestaurantComponent {
		this: AgriculturalComponent with LivestockComponent =>

		val restaurant: Restaurant

		case class Restaurant(potatoFarm: PotatoFarm, meatery: Meatery) {
			def orderSteakWithPotatoes(): Unit = {
				println(s"Welcome to $this. Here's your order!")
			}
		}
	}

	val app = new RestaurantComponent with LivestockComponent with TraditionalAgriculturalComponent {
		override lazy val potatoFarm = TraditionalPotatoFarm(field, digger)
		override lazy val field = Field()
		override lazy val digger = Digger()
		override lazy val cowPasture = CowPasture(potatoFarm)
		override lazy val meatery = Meatery(cowPasture)
		override lazy val restaurant = Restaurant(potatoFarm, meatery)
	}

	app.restaurant.orderSteakWithPotatoes()

	val testing = new RestaurantComponent with LivestockComponent with AgriculturalComponent {
		override lazy val potatoFarm = new PotatoFarm {} // some kind of mocking framework used here
		override lazy val cowPasture = CowPasture(potatoFarm)
		override lazy val meatery = Meatery(cowPasture)
		override lazy val restaurant = Restaurant(potatoFarm, meatery)
	}

	testing.restaurant.orderSteakWithPotatoes()
}
