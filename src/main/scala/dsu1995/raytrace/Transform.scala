package dsu1995.raytrace


case class Transform(mat: Mat4, inv: Mat4) {
  def scale(factor: Vec3): Transform = {
    val invScaleMatrix = Mat4().scale(
      Vec3(
        1 / factor.x,
        1 / factor.y,
        1 / factor.z
      )
    )

    Transform(
      mat = mat.scale(factor),
      inv = inv * invScaleMatrix
    )
  }

  def scale(factor: Double): Transform = scale(Vec3(factor, factor, factor))

  def translate(delta: Vec3): Transform = {
    Transform(
      mat = mat.translate(delta),
      inv = inv * Mat4().translate(-delta)
    )
  }

  import Axis.Axis

  def rotate(axis: Axis, radians: Double): Transform = {
    Transform(
      mat = mat.rotate(axis, radians),
      inv = inv * Mat4().rotate(axis, -radians)
    )
  }
}

object Transform {
  def apply(mat: Mat4, inv: Mat4): Transform = new Transform(mat, inv)
  def apply(): Transform = new Transform(Mat4(), Mat4())
}