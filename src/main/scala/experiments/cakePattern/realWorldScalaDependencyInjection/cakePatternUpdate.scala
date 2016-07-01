package experiments.cakePattern.realWorldScalaDependencyInjection

object cakePatternUpdate extends App {

	trait OnOffDeviceComponent {
		val onOff: OnOffDevice

		trait OnOffDevice {
			def on(): Unit
			def off(): Unit
		}
	}

	trait SensorDeviceComponent {
		val sensor: SensorDevice

		trait SensorDevice {
			def isCoffeePresent: Boolean
		}
	}

	trait HeaterComponent extends OnOffDeviceComponent {
		class Heater extends OnOffDevice {
			def on() = println("heater.on")
			def off() = println("heater.off")
		}
	}

	trait PotSensorComponent extends SensorDeviceComponent {
		class PotSensor extends SensorDevice {
			def isCoffeePresent = true
		}
	}

	trait WarmerComponent {
		this: SensorDeviceComponent with OnOffDeviceComponent =>

		class Warmer {
			def trigger() = {
				if (sensor.isCoffeePresent) onOff.on()
				else onOff.off()
			}
		}
	}

	object ComponentRegistry extends HeaterComponent
																	 with PotSensorComponent
																	 with WarmerComponent {
		val onOff = new Heater
		val sensor = new PotSensor
		val warmer = new Warmer
	}

	val warmer = ComponentRegistry.warmer
	warmer.trigger()
}
