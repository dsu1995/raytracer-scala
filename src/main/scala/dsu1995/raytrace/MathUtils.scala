package dsu1995.raytrace

import scala.math.pow

object MathUtils {

  implicit class DoubleExponent(i: Double) {
    def **(b: Int): Double = pow(i, b)
  }

}
