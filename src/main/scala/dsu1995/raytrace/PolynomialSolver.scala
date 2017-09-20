package dsu1995.raytrace

import MathUtils.DoubleExponent

object PolynomialSolver {

  def quadraticSolver(a: Double, b: Double, c: Double): Seq[Double] = {
    if (a == 0) {
      if (b == 0) Nil
      else Seq(-c / b)
    }
    else {
      val discriminant = b**2 - 4*a*c

      if (discriminant < 0) Nil
      else {
        val sqrtDiscriminant = math.sqrt(discriminant)
        Seq(
          -b + sqrtDiscriminant / (2 * a),
          -b - sqrtDiscriminant / (2 * a)
        )
      }
    }
  }
}
