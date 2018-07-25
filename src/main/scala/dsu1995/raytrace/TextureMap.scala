package dsu1995.raytrace

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

import scala.math.floor

case class TextureMap(
  fileName: String,
  xratio: Double = 1,
  yratio: Double = 1
) {
  private val image: BufferedImage = ImageIO.read(new File(fileName))
  private val width = image.getWidth
  private val height = image.getHeight

  /**
    * @param x between 0.0 and 1.0
    * @param y between 0.0 and 1.0
    * @return
    */
  def getPixel(x: Double, y: Double): Vec3 = {
    assert(0.0 <= x && x <= 1.0 && 0.0 <= y && y <= 1.0)

    val x2 = x * xratio - floor(x * xratio)
    val y2 = y * yratio - floor(y * yratio)

    val x3 = floor(x2 * (width - 1)).toInt
    val y3 = floor(y2 * (height - 1)).toInt

    val intColour = image.getRGB(x3, y3)
    val colour = new Color(intColour)

    Vec3(
      colour.getRed / 255.0,
      colour.getGreen / 255.0,
      colour.getBlue / 255.0
    )
  }
}