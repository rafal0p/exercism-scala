object SpaceAge {
  val onEarth: Double => Double = ageInYearsOnPlanetWith()

  val onMercury: Double => Double = ageInYearsOnPlanetWith(0.2408467)

  val onVenus: Double => Double = ageInYearsOnPlanetWith(0.61519726)

  val onMars: Double => Double = ageInYearsOnPlanetWith(1.8808158)

  val onJupiter: Double => Double = ageInYearsOnPlanetWith(11.862615)

  val onSaturn: Double => Double = ageInYearsOnPlanetWith(29.447498)

  val onUranus: Double => Double = ageInYearsOnPlanetWith(84.016846)

  val onNeptune: Double => Double = ageInYearsOnPlanetWith(164.79132)

  private def ageInYearsOnPlanetWith(orbitalPeriod: Double = 1)(ageInSeconds: Double) =
    ageInSeconds / yearInSecondsOnPlanetWith(orbitalPeriod)

  private def yearInSecondsOnPlanetWith(orbitalPeriod: Double) = orbitalPeriod * earthYearInSeconds

  private val earthYearInSeconds = 365.25 * 24 * 60 * 60
}