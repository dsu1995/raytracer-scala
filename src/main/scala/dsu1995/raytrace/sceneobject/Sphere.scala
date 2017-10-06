package dsu1995.raytrace.sceneobject

import dsu1995.raytrace._

import MathUtils.DoubleExponent

/**
  * Unit sphere centered at (0, 0, 0)
  */
case class Sphere(
  transform: Transform,
  material: Material,
  texture: Option[TextureMap],
  normalMap: Option[NormalMap]
) extends Primitive {

  import Sphere._

  override protected
  def getCSGSegmentsTransformed(ray: Ray): Seq[LineSegment] = {
    val A = ray.direction.length2
    val B = 2 * (ray.direction dot (ray.origin - center))
    val C = (ray.origin - center).length2 - radius**2

    val roots = PolynomialSolver.quadraticSolver(A, B, C)

    val intersections = for {
      t <- roots
      if t >= 0
    } yield {
      val point = ray.origin + ray.direction * t
      Intersection(
        point = point,
        normal = calculateNormal(point),
        objCenter = center,
        material = getMaterial(point)
      )
    }

    intersections match {
      case Nil => Nil
      case Seq(intersection) => Seq(LineSegment(intersection, intersection))
      case Seq(near, far) => Seq(LineSegment(near, far))
      case _ => throw new RuntimeException(s"Ray intersected $name at more than 2 places: $intersections")
    }
  }

  private def calculateNormal(point: Vec3): Vec3 = {
    point - center
  }

  private def getMaterial(point: Vec3): Material = {
    material
  }
}

object Sphere {
  val name: String = "Sphere"

  val center: Vec3 = Vec3(0, 0, 0)
  val radius: Double = 1
}

