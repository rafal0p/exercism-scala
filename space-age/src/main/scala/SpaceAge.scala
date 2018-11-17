object SpaceAge {
  def onEarth(ageInSeconds: Double): Double = ageInYearsOnPlanetWith(1, ageInSeconds)

  def onMercury(ageInSeconds: Double): Double = ageInYearsOnPlanetWith(0.2408467, ageInSeconds)

  def onVenus(ageInSeconds: Double): Double = ageInYearsOnPlanetWith(0.61519726, ageInSeconds)

  def onMars(ageInSeconds: Double): Double = ageInYearsOnPlanetWith(1.8808158, ageInSeconds)

  def onJupiter(ageInSeconds: Double): Double = ageInYearsOnPlanetWith(11.862615, ageInSeconds)

  def onSaturn(ageInSeconds: Double): Double = ageInYearsOnPlanetWith(29.447498, ageInSeconds)

  def onUranus(ageInSeconds: Double): Double = ageInYearsOnPlanetWith(84.016846, ageInSeconds)

  def onNeptune(ageInSeconds: Double): Double = ageInYearsOnPlanetWith(164.79132, ageInSeconds)

  private def ageInYearsOnPlanetWith(orbitalPeriod: Double, ageInSeconds: Double) =
    ageInSeconds / yearInSecondsOnPlanetWith(orbitalPeriod)

  private def yearInSecondsOnPlanetWith(orbitalPeriod: Double) = orbitalPeriod * earthYearInSeconds

  private val earthYearInSeconds = 365.25 * 24 * 60 * 60
}