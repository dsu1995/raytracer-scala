package dsu1995.raytrace

case class Intersection(
  point: Vec3,
  normal: Vec3,
  objectCenter: Vec3,
  material: Material
)

case class LineSegment(
  near: Intersection,
  far: Intersection
)