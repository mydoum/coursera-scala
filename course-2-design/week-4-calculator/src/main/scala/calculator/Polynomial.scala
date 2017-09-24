package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - (4.0 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    delta() match {
      case d if d > 0.0 => {
        val first = (-b() -  Math.sqrt(d)) / (2 * a())
        val second = (-b() +  Math.sqrt(d)) / (2 * a())
        Signal(Set(first, second))
      }
      case d if d == 0.0 => Signal(Set(-b() / (2 * a())))
      case _ => Signal(Set())
    }
  }
}
