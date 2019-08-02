package experiments.cakePattern.realWorldScalaDependencyInjection

object implicitDeclarations extends App {

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

	class Warmer(implicit val sensor: SensorDevice,
							 implicit val onOff: OnOffDevice) {
		def trigger() = {
			if (sensor.isCoffeePresent) onOff.on()
			else onOff.off()
		}
	}

	object Services {
		implicit val potSensor = new PotSensor
		implicit val heater = new Heater
	}

	import Services._

	val warmer = new Warmer()
	warmer.trigger()
}
