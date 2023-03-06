package main

object engines {
  def main(args: Array[String]): Unit = {
    val rocket = new Rocket(Header = new RocketHeader, IEngine = new hatersEngine)
    println("Haters Power:" + specialNasaMethod(rocket.IEngine.power, rocket.weight))
    rocket.IEngine = cryEngine()
    println("Haters Power:" + specialNasaMethod(rocket.IEngine.power, rocket.weight))
  }

  def specialNasaMethod(power: Int, mass: Int) = (mass / power * 10) + 82

  trait Engine {
    val weight: Int
    val power: Int

    def start(): Unit

    def stop(): Unit
  }

  case class hatersEngine() extends Engine {
    val weight = 322
    val power = 228
    val getCop = "ExtremeCode TV"
    println("hatersEngine is created")
    def start(): Unit = println("Haters engine started")

    def stop(): Unit = println("Haters engine stopped")
  }

  case class cryEngine() extends Engine {
    val weight = 800
    val power = 2000
    val getCop = "ExtremeCode TV"
    println("cryEngine is created")
    def start(): Unit = println("cryEngine engine started")

    def stop(): Unit = println("cryEngine engine stopped")
  }

  class Rocket(val Header: RocketHeader, var IEngine: Engine) {


    val weight: Int = Header.getWeight + IEngine.weight
  }

  class RocketHeader {
    val cosmonauts = 3
    val massShell = 5000

    val getWeight: Int = cosmonauts * 3 + massShell

    def sendMessage(message: String): Unit = {
      println("Сообщение:")
      println(message)
      println("отправлено")
    }
  }


}
