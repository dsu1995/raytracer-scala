package dsu1995.raytrace.sceneobject

import dsu1995.raytrace.{Material, NormalMap, TextureMap}

trait Primitive extends SceneNode {
  val material: Material
  val texture: Option[TextureMap]
  val normalMap: Option[NormalMap]

  //  override protected def getClosestIntersectionTransformed(transformedRay: Ray) = {
  //    ???
  //  }
}
