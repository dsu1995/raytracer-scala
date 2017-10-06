package dsu1995.raytrace

case class Material(
  kd: Vec3,
  ks: Vec3,
  shininess: Double = 0,
  glossiness: Double = 0,
  reflectivity: Double = 0,
  transparency: Double = 0,
  refractiveIndex: Double = 1
)