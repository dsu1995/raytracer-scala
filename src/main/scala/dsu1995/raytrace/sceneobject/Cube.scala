package dsu1995.raytrace.sceneobject

import dsu1995.raytrace._

/**
  * Axis aligned cube with side length 2 centered at (0, 0, 0)
  */
case class Cube(
  transform: Transform,
  material: Material,
  texture: Option[TextureMap],
  normalMap: Option[NormalMap]
) extends Primitive {

  import Cube._

  override protected
  def getCSGSegmentsTransformed(ray: Ray): Seq[LineSegment] = {
    val intersections = axes.flatMap { axis =>
      directions.flatMap { direction =>
        val face: Vec3 = axis * direction

        val t = (direction - (axis dot ray.origin)) / (axis dot ray.direction)
        if (t >= 0) {
          val point: Vec3 = ray.origin + ray.direction * t

          val isInFace: Boolean = axes
            .filter { otherAxis => otherAxis != axis }
            .forall { otherAxis =>
              val component = otherAxis dot point
              min <= component && component <= max
            }

          if (isInFace) Some(
            Intersection(
              point = point,
              normal = face,
              objCenter = objCenter,
              material = material
            )
          )
          else None
        }
        else None
      }
    }

    intersections match {
      case Nil => Nil
      case Seq(intersection) => Seq(LineSegment(intersection, intersection))
      case Seq(near, far) => Seq(LineSegment(near, far))
      case _ => throw new RuntimeException(s"Ray intersected $name at more than 2 places: $intersections")
    }
  }
}

object Cube {
  val name: String = "Cube"

  val axes = Seq(Vec3(1, 0, 0), Vec3(0, 1, 0), Vec3(0, 0, 1))
  val directions = Seq(1, -1)

  val objCenter = Vec3(0, 0, 0)

  val min: Int = -1
  val max: Int = 1
}
