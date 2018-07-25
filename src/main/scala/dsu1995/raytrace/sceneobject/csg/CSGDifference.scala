package dsu1995.raytrace.sceneobject.csg

import dsu1995.raytrace.sceneobject.SceneNode
import dsu1995.raytrace.{LineSegment, Ray, Transform}

case class CSGDifference(
  transform: Transform,
  left: SceneNode,
  right: SceneNode
) extends CSGOperator {

  override protected
  def getCSGSegmentsTransformed(ray: Ray): Seq[LineSegment] = {
    val leftSegments = left.getCSGSegments(ray).toList
    val rightSegments = right.getCSGSegments(ray).toList

    // TODO should probably make tail recursive
    def merge(left: List[LineSegment], right: List[LineSegment]): List[LineSegment] = {
      (left, right) match {
        case (Nil, _) => Nil
        case (l, Nil) => l
        case ((lhead @ LineSegment(lnear, lfar)) :: lrest, (rhead @ LineSegment(rnear, rfar)) :: rrest) =>
          val lNearDist = (ray.origin - lnear.point).length2
          val lFarDist = (ray.origin - lfar.point).length2

          val rNearDist = (ray.origin - rnear.point).length2
          val rFarDist = (ray.origin - rfar.point).length2

          /*
           * l      ----
           * r ----
           */
          if (rFarDist <= lNearDist) merge(left, rrest)
          /*
           * l    ----
           * r  ----
           */
          else if (rNearDist <= lNearDist && lNearDist <= rFarDist && rFarDist <= lFarDist)
            merge(
              left = lhead.copy(near = rfar.copy(normal = -rfar.normal)) :: lrest,
              right = rrest
            )
          /*
           * l    ----
           * r  --------
           */
          else if (rNearDist <= lNearDist && lFarDist <= rFarDist) merge(lrest, right)
          /*
           * l    ----
           * r     --
           */
          else if (lNearDist <= rNearDist && rFarDist <= lFarDist) {
            val segment = lhead.copy(far = rnear.copy(normal = -rnear.normal))
            segment :: merge(
              left = lhead.copy(near = rfar.copy(normal = -rfar.normal)) :: lrest,
              right = rrest
            )
          }
          /*
           * l    ----
           * r      ----
           */
          else if (lNearDist <= rNearDist && rNearDist <= lFarDist && lFarDist <= rFarDist) {
            val segment = lhead.copy(far = rnear.copy(normal = -rnear.normal))
            segment :: merge(lrest, right)
          }
          /*
           * l  ----
           * r        ----
           */
          else if (lFarDist <= rNearDist) lhead :: merge(lrest, right)
          else throw new AssertionError("Impossible")
      }
    }

    merge(leftSegments, rightSegments)
  }
}
