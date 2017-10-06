package dsu1995.raytrace

import dsu1995.raytrace.sceneobject.SceneNode

case class Tracer(
  root: SceneNode,
  imageDims: Vec2,
  eye: Vec3,
  view: Vec3,
  up: Vec3,
  fovYDegs: Double,
  ambient: Vec3,
  lights: Seq[Light]
) {

  val NEAR_PLANE_DISTANCE: Double = 1

  val EPS: Double = 0.0000001

  val RECURSION_DEPTH: Int = 8

  val AIR_INDEX_OF_REFRACTION: Double = 1

  private val MVW: Mat4 = {
    val T1 = Mat4().translate(
      Vec3(imageDims / (-2), NEAR_PLANE_DISTANCE)
    )
    val h = 2 * NEAR_PLANE_DISTANCE * math.tan(math.toRadians(fovYDegs) / 2)
    val S2 = Mat4().scale(
      Vec3(-h / imageDims.y, -h / imageDims.y, 1)
    )
    val w = view.normalize
    val u = (this.up cross w).normalize
    val v = w cross u
    val R3 = Mat4(
      Vec4(u, 0),
      Vec4(v, 0),
      Vec4(w, 0),
      Vec4(0, 0, 0, 1)
    )
    val T4 = Mat4().translate(eye)
    T4 * R3 * S2 * T1
  }


  def render: Seq[Seq[Vec3]] = {

  }

  private def renderPixel(x: Double, y: Double): Vec3 = {
    val screenCoordPixel = Vec4(x, y, 0, 1)
    val worldCoordPixel = MVW * screenCoordPixel
    val ray = Ray(
      origin = eye,
      direction = worldCoordPixel.toVec3 - eye
    )

    traceRecursive(ray, RECURSION_DEPTH)
  }

  private def traceRecursive(ray: Ray, recursionDepth: Int): Vec3 = {
    val intersection: Option[Intersection] = traceRay(ray)

    intersection match {
      case None => backgroundColour(ray)
      case Some(i) =>

    }

  }

  private def backgroundColour(ray: Ray): Vec3 = {

  }

  private def traceRay(ray: Ray): Option[Intersection] = {

  }

//  def perturb(v: Vec3, maxDeltaDegs: Double): Vec3 = {
//    val vhat = v.normalize
//    val theta = math.acos(vhat.z)
//    val phi = math.atan2(vhat.y, vhat.x)
//
//
//  }



}
