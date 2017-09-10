package dsu1995.raytrace

case class SceneNode(
    name: String,
    obj: SceneObject,
    transform: Transform,
    children: SceneNode*
) {

}
