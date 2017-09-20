package dsu1995.raytrace.sceneobject

import dsu1995.raytrace.{LineSegment, _}


trait SceneNode {
  val transform: Transform

  //  final def getClosestIntersection(ray: Ray): Option[Intersection] = {
  //    val transformedRay = ???
  //    getClosestIntersection(transformedRay).map { intersection =>
  //      val untransformedIntersection = ???
  //      untransformedIntersection
  //    }
  //  }
  //
  //  protected def getClosestIntersectionTransformed(transformedRay: Ray): Option[Intersection]

  final def getCSGSegments(ray: Ray): Seq[LineSegment] = {
    val transformedRay = Ray(
      origin = (transform.inv * Vec4(ray.origin, 1)).toVec3,
      direction = (transform.inv * Vec4(ray.direction, 0)).toVec3
    )

    def untransformIntersection(intersection: Intersection): Intersection = {
      Intersection(
        point = (transform.mat * Vec4(intersection.point, 1)).toVec3,
        normal = transform.inv.toMat3.transpose * intersection.normal,
        objCenter = (transform.mat * Vec4(intersection.objCenter, 1)).toVec3,
        material = intersection.material
      )
    }

    getCSGSegmentsTransformed(transformedRay)
      .map { case LineSegment(near, far) =>
        LineSegment(
          near = untransformIntersection(near),
          far = untransformIntersection(far)
        )
      }
  }

  protected def getCSGSegmentsTransformed(transformedRay: Ray): Seq[LineSegment]
}
