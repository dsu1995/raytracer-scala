package dsu1995.raytrace.sceneobject

import dsu1995.raytrace._


/**
  * Cylinder centered at (0, 0, 0), with radius = 1, height = 2
  */
case class Cylinder(
  transform: Transform,
  material: Material,
  texture: Option[Texture],
  normalMap: Option[NormalMap]
) extends Primitive {

  import Cylinder._

  override protected
  def getCSGSegmentsTransformed(ray: Ray): Seq[LineSegment] = {
    val sideIntersections = {
      val A = ray.direction.xy.length2
      val B = 2 * (ray.direction.xy dot ray.origin.xy)
      val C = ray.origin.xy.length2 - 1
      val roots = PolynomialSolver.quadraticSolver(A, B, C)

      roots
        .filter { t => t >= 0 }
        .map { t => ray.origin + ray.direction * t }
        .filter { point => zmin < point.z && point.z < zmax }
        .map { point =>
          Intersection(
            point = point,
            normal = point * Vec3(1, 1, 0),
            objCenter = objCenter,
            material = material
          )
        }
    }

    val topBottomIntersections = Seq(zmin, zmax).flatMap { zval =>
      val t = (zval - ray.origin.z) / ray.direction.z
      if (t >= 0) {
        val point = ray.origin + ray.direction * t

        if (point.xy.length2 <= 1) Some(
          Intersection(
            point = point,
            normal = Vec3(0, 0, 1) * zval,
            objCenter = objCenter,
            material = material
          )
        )
        else None
      }
      else None
    }

    sideIntersections ++ topBottomIntersections match {
      case Nil => Nil
      case Seq(intersection) => Seq(LineSegment(intersection, intersection))
      case Seq(near, far) => Seq(LineSegment(near, far))
      case intersections => throw new RuntimeException(s"Ray intersected $name at more than 2 places: $intersections")
    }
  }
}

object Cylinder {
  val name: String = "Cylinder"

  val objCenter = Vec3(0, 0, 0)

  val zmin: Int = -1
  val zmax: Int = 1
}