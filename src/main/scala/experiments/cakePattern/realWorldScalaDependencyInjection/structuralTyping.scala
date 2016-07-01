package experiments.cakePattern.realWorldScalaDependencyInjection

object structuralTyping extends App {

	// service interfaces
	trait OnOffDevice {
		def on(): Unit
		def off(): Unit
	}

	trait SensorDevice {
		def isCoffeePresent: Boolean
	}

	// service implementations
	class Heater extends OnOffDevice {
		def on() = println("heater on")
		def off() = println("heater off")
	}

	class PotSensor extends SensorDevice {
		def isCoffeePresent: Boolean = true
	}

	// service declares two dependencies that it wants injected
	// uses structural typing to declare its dependencies
	class Warmer(env: {
		val potSensor: SensorDevice
		val heater: OnOffDevice
	}) {
		def trigger() = {
			if (env.potSensor.isCoffeePresent) env.heater.on()
			else env.heater.off()
		}
	}

	class Client(env: { val warmer: Warmer }) {
		env.warmer.trigger
	}

	// instantiate services in a configuration module
	object Config {
		lazy val potSensor = new PotSensor
		lazy val heater = new Heater
		lazy val warmer = new Warmer(this) // here the injection happens
	}

	new Client(Config)
}
