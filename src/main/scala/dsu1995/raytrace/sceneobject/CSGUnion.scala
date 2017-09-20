package dsu1995.raytrace.sceneobject

import dsu1995.raytrace.{LineSegment, Ray, Transform}

case class CSGUnion(
  transform: Transform,
  nodes: SceneNode*
) extends CSGOperator {

  override protected
  def getCSGSegmentsTransformed(ray: Ray): Seq[LineSegment] = {

    def merge(left: List[LineSegment], right: List[LineSegment]): List[LineSegment] = {
      (left, right) match {
        case (Nil, Nil) => Nil
        case (l, Nil) => l
        case (Nil, r) => r
        case (
          (lhead @ LineSegment(lnear, lfar)) :: lrest,
          (rhead @ LineSegment(rnear, rfar)) :: rrest
          ) =>
          val lNearDist = (ray.origin - lnear.point).length2
          val lFarDist = (ray.origin - lfar.point).length2

          val rNearDist = (ray.origin - rnear.point).length2
          val rFarDist = (ray.origin - rfar.point).length2

          /*
           * l      ----
           * r ----
           */
          if (rFarDist <= lNearDist) rhead :: merge(left, rrest)
          /*
           * l    ----
           * r  ----
           */
          else if (rNearDist <= lNearDist && lNearDist <= rFarDist && rFarDist <= lFarDist) {
            merge(LineSegment(rnear, lfar) :: lrest, rrest)
          }
          /*
           * l    ----
           * r  --------
           */
          else if (rNearDist <= lNearDist && lFarDist <= rFarDist) {
            merge(lrest, right)
          }
          /*
           * l    ----
           * r     --
           */
          else if (lNearDist <= rNearDist && rFarDist <= lFarDist) {
            merge(left, rrest)
          }
          /*
           * l    ----
           * r      ----
           */
          else if (lNearDist <= rNearDist && rNearDist <= lFarDist && lFarDist <= rFarDist) {
            merge(lrest, LineSegment(lnear, rfar) :: rrest)
          }
          /*
           * l  ----
           * r        ----
           */
          else if (lFarDist <= rNearDist) lhead :: merge(lrest, right)
          else throw new AssertionError("Impossible")
      }
    }

    nodes
      .map { node => node.getCSGSegments(ray).toList }
      .fold(Nil)(merge)
  }
}
