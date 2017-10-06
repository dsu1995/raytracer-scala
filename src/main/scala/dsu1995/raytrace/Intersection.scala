package dsu1995.raytrace

case class Intersection(
  point: Vec3,
  normal: Vec3,
  objCenter: Vec3,
  material: Material
)

object Intersection {
  def min(ray: Ray)(i1: Intersection, i2: Intersection): Intersection = {
    if ((ray.origin - i1.point).length2 < (ray.origin - i2.point).length2) i1
    else i2
  }
}


case class LineSegment(
  near: Intersection,
  far: Intersection
)