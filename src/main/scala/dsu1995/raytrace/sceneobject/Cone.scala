package dsu1995.raytrace.sceneobject

import dsu1995.raytrace._


/**
  * Models an inverted cone with its tip at (0,0,0),
  * with base radius = 1, and height 1
  */
case class Cone(
  transform: Transform,
  material: Material,
  texture: Option[TextureMap],
  normalMap: Option[NormalMap]
) extends Primitive {

  import Cone._

  override protected
  def getCSGSegmentsTransformed(ray: Ray): Seq[LineSegment] = {
    val sideIntersections = {
      val A = (ray.direction * ray.direction * Vec3(1, 1, -1)).toSeq.sum
      val B = 2 * (ray.direction * ray.origin * Vec3(1, 1, -1)).toSeq.sum
      val C = (ray.origin * ray.origin * Vec3(1, 1, -1)).toSeq.sum
      val roots = PolynomialSolver.quadraticSolver(A, B, C)

      roots
        .filter { t => t >= 0 }
        .map { t => ray.origin + ray.direction * t }
        .filter { point => zmin <= point.z && point.z <= zmax }
        .map { point =>
          Intersection(
            point = point,
            normal = point * Vec3(2, 2, -1),
            objCenter = objCenter,
            material
          )
        }
    }

    val topIntersection = {
      val t = (zmax - ray.origin.z) / ray.direction.z
      if (t >= 0) {
        val point = ray.origin + ray.direction * t

        if (point.xy.length2 <= 1) Some(
          Intersection(
            point = point,
            normal = up,
            objCenter = objCenter,
            material = material
          )
        )
        else None
      }
      else None
    }

    sideIntersections ++ topIntersection match {
      case Nil => Nil
      case Seq(intersection) => Seq(LineSegment(intersection, intersection))
      case Seq(near, far) => Seq(LineSegment(near, far))
      case intersections => throw new RuntimeException(s"Ray intersected $name at more than 2 places: $intersections")
    }
  }
}

object Cone {
  val name: String = "Cone"

  val objCenter: Vec3 = Vec3(0, 0, 0.5)

  val zmin: Int = 0
  val zmax: Int = 1

  val up: Vec3 = Vec3(0, 0, 1)
}