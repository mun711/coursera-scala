package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(delta() match {
      case d if d < 0 => Set.empty
      case d if d == 0 => Set(-b()/(2*a()))
      case _ => Set((-b()-math.sqrt(delta()))/(2*a()) , (-b()+math.sqrt(delta()))/(2*a()))
    })    
  }
}
