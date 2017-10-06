package dsu1995.raytrace



case class Vec2(x: Double, y: Double) {
  def unary_- : Vec2 = Vec2(-x, -y)

  def +(that: Vec2): Vec2 = {
    Vec2(this.x + that.x, this.y + that.y)
  }

  def -(that: Vec2): Vec2 = this + (-that)

  def *(scalar: Double): Vec2 = Vec2(x * scalar, y * scalar)
  def *(that: Vec2): Vec2 =  {
    Vec2(this.x * that.x, this.y * that.y)
  }

  def /(scalar: Double): Vec2 = this * (1 / scalar)

  def dot(that: Vec2): Double = {
    this.x * that.x + this.y * that.y
  }

  def normalize: Vec2 = Vec2(x / length, y / length)

  lazy val length2: Double = this dot this
  lazy val length: Double = math.sqrt(length2)

  def toSeq: Seq[Double] = Seq(x, y)
}

object Vec2 {
  def apply(): Vec2 = new Vec2(0, 0)
  def apply(x: Double) = new Vec2(x, x)
  def apply(x: Double, y: Double): Vec2 = new Vec2(x, y)
}



case class Vec3(x: Double, y: Double, z: Double) {
  def unary_- : Vec3 = Vec3(-x, -y, -z)

  def +(that: Vec3): Vec3 = {
    Vec3(this.x + that.x, this.y + that.y, this.z + that.z)
  }

  def -(that: Vec3): Vec3 = this + (-that)

  def *(scalar: Double): Vec3 = Vec3(x * scalar, y * scalar, z * scalar)
  def *(that: Vec3): Vec3 = {
    Vec3(this.x * that.x, this.y * that.y, this.z * that.z)
  }

  def /(scalar: Double): Vec3 = this * (1 / scalar)

  def dot(that: Vec3): Double = {
    this.x * that.x + this.y * that.y + this.z * that.z
  }

  def cross(that: Vec3): Vec3 = Vec3(
    this.y * that.z - this.z * that.y,
    this.z * that.x - this.x * that.z,
    this.x * that.y - this.y * that.x
  )

  def normalize: Vec3 = Vec3(x / length, y / length, z / length)

  lazy val length2: Double = this dot this
  lazy val length: Double = math.sqrt(length2)

  def xy: Vec2 = Vec2(x, y)

  def toSeq: Seq[Double] = Seq(x, y, z)
}

object Vec3 {
  def apply(): Vec3 = new Vec3(0, 0, 0)
  def apply(x: Double) = new Vec3(x, x, x)
  def apply(x: Double, y: Double, z: Double): Vec3 = new Vec3(x, y, z)
  def apply(v: Vec2, z: Double): Vec3 = new Vec3(v.x, v.y, z)
}





case class Vec4(x: Double, y: Double, z: Double, w: Double) {
  def unary_- : Vec4 = Vec4(-x, -y ,-z, -w)

  def toVec3: Vec3 = Vec3(x / w, y / w, z / w)
}

object Vec4 {
  def apply(): Vec4 = new Vec4(0, 0, 0, 0)
  def apply(x: Double) = new Vec4(x, x, x, x)
  def apply(x: Double, y: Double, z: Double, w: Double): Vec4 = new Vec4(x, y, z, w)
  def apply(vec3: Vec3, w: Double) = new Vec4(vec3.x, vec3.y, vec3.z, w)
}




case class RowVec3(x: Double, y: Double, z: Double) {
  def *(that: Vec3): Double = {
    this.x * that.x + this.y * that.y + this.z * that.z
  }

  def transpose: Vec3 = Vec3(x, y, z)
}

object RowVec3 {
  def apply(x: Double, y: Double, z: Double): RowVec3 = new RowVec3(x, y, z)
}





case class RowVec4(x: Double, y: Double, z:Double, w: Double) {
  def *(that: Vec4): Double = {
    this.x * that.x + this.y * that.y + this.z * that.z + this.w * that.w
  }

  def transpose: Vec4 = Vec4(x, y, z, w)
}

object RowVec4 {
  def apply(x: Double, y: Double, z: Double, w: Double): RowVec4 = new RowVec4(x, y, z, w)
}





case class Mat3(col0: Vec3, col1: Vec3, col2: Vec3) {
  private lazy val row0: RowVec3 = RowVec3(col0.x, col1.x, col2.x)
  private lazy val row1: RowVec3 = RowVec3(col0.y, col1.y, col2.y)
  private lazy val row2: RowVec3 = RowVec3(col0.z, col1.z, col2.z)

  def *(that: Vec3): Vec3 = Vec3(row0 * that, row1 * that, row2 * that)
  def *(that: Mat3): Mat3 = Mat3(this * that.col0, this * that.col1, this * that.col2)

  def toMat4: Mat4 = Mat4(
    Vec4(col0, 0),
    Vec4(col1, 0),
    Vec4(col2, 0),
    Vec4(0, 0, 0, 1)
  )

  def transpose: Mat3 = Mat3(row0.transpose, row1.transpose, row2.transpose)
}

object Axis extends Enumeration {
  type Axis = Value
  val X, Y, Z = Value
}

object Mat3 {
  def apply(col0: Vec3, col1: Vec3, col2: Vec3): Mat3 = new Mat3(col0, col1, col2)

  def apply(): Mat3 = new Mat3(
    col0 = Vec3(1, 0, 0),
    col1 = Vec3(0, 1, 0),
    col2 = Vec3(0, 0, 1)
  )
}





case class Mat4(col0: Vec4, col1: Vec4, col2: Vec4, col3: Vec4) {
  private lazy val row0: RowVec4 = RowVec4(col0.x, col1.x, col2.x, col3.x)
  private lazy val row1: RowVec4 = RowVec4(col0.y, col1.y, col2.y, col3.y)
  private lazy val row2: RowVec4 = RowVec4(col0.z, col1.z, col2.z, col3.z)
  private lazy val row3: RowVec4 = RowVec4(col0.w, col1.w, col2.w, col3.w)

  def *(that: Vec4): Vec4 = Vec4(row0 * that, row1 * that, row2 * that, row3 * that)
  def *(that: Mat4): Mat4 = Mat4(this * that.col0, this * that.col1, this * that.col2, this * that.col3)

  def toMat3: Mat3 = Mat3(col0.toVec3, col1.toVec3, col2.toVec3)

  def scale(factor: Vec3): Mat4 = {
    val scaleMatrix = Mat4(
      Vec4(factor.x, 0, 0, 0),
      Vec4(0, factor.y, 0, 0),
      Vec4(0, 0, factor.z, 0),
      Vec4(0, 0, 0, 1)
    )

    scaleMatrix * this
  }

  def transpose: Mat4 = Mat4(row0.transpose, row1.transpose, row2.transpose, row3.transpose)

  def scale(factor: Double): Mat4 = scale(Vec3(factor))

  def translate(delta: Vec3): Mat4 = {
    val translateMatrix = Mat4(
      Vec4(1, 0, 0, 0),
      Vec4(0, 1, 0, 0),
      Vec4(0, 0, 1, 0),
      Vec4(delta, 1)
    )

    translateMatrix * this
  }

  import Axis.Axis

  def rotate(axis: Axis, radians: Double): Mat4 = {
    val cosTheta = math.cos(radians)
    val sinTheta = math.sin(radians)

    val rotationMatrix = axis match {
      case Axis.X => Mat3(
        Vec3(1, 0, 0),
        Vec3(0, cosTheta, sinTheta),
        Vec3(0, -sinTheta, cosTheta)
      )
      case Axis.Y => Mat3(
        Vec3(cosTheta, 0, -sinTheta),
        Vec3(0, 1, 0),
        Vec3(sinTheta, 0, cosTheta)
      )
      case Axis.Z => Mat3(
        Vec3(cosTheta, sinTheta, 0),
        Vec3(-sinTheta, cosTheta, 0),
        Vec3(0, 0, 1)
      )
    }

    rotationMatrix.toMat4 * this
  }
}

object Mat4 {
  def apply(col0: Vec4, col1: Vec4, col2: Vec4, col3: Vec4): Mat4 = new Mat4(col0, col1, col2, col3)

  def apply(): Mat4 = new Mat4(
    col0 = Vec4(1, 0, 0, 0),
    col1 = Vec4(0, 1, 0, 0),
    col2 = Vec4(0, 0, 1, 0),
    col3 = Vec4(0, 0, 0, 1)
  )
}
