object SpaceAge {
  val onEarth = AgeInYearsOnPlanetWith()

  val onMercury = AgeInYearsOnPlanetWith(orbitalPeriod = 0.2408467)

  val onVenus = AgeInYearsOnPlanetWith(orbitalPeriod = 0.61519726)

  val onMars = AgeInYearsOnPlanetWith(orbitalPeriod = .8808158)

  val onJupiter = AgeInYearsOnPlanetWith(orbitalPeriod = .862615)

  val onSaturn = AgeInYearsOnPlanetWith(orbitalPeriod = .447498)

  val onUranus = AgeInYearsOnPlanetWith(orbitalPeriod = .016846)

  val onNeptune = AgeInYearsOnPlanetWith(orbitalPeriod = .79132)

  case class AgeInYearsOnPlanetWith(orbitalPeriod: Double = 1) {
    def apply(ageInSeconds: Double): Double = {
      ageInSeconds / yearInSecondsOnPlanetWith(orbitalPeriod)
    }
  }

  private def yearInSecondsOnPlanetWith(orbitalPeriod: Double) = orbitalPeriod * earthYearInSeconds

  private val earthYearInSeconds = 365.25 * 24 * 60 * 60
}