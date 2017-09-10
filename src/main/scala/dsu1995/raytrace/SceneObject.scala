package dsu1995.raytrace

trait SceneObject {
  val transform: Transform
  val material: Option[Material]
  val texture: Option[Texture]
  val normalMap: Option[NormalMap]


}
