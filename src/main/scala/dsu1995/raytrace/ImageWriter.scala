package dsu1995.raytrace

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

object ImageWriter {

  def write(colours: Seq[Seq[Vec3]], fileName: String): Unit = {

    val height = colours.length
    val width = colours.head.length

    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

    colours.zipWithIndex.foreach { case (row, rowId) =>
      row.zipWithIndex.foreach { case (pixel, colId) =>
        val colour = new Color(
          (pixel.x * 255).toInt,
          (pixel.y * 255).toInt,
          (pixel.z * 255).toInt
        )

        bufferedImage.setRGB(rowId, colId, colour.getRGB)
      }
    }

    ImageIO.write(bufferedImage, "png", new File(fileName))
  }
}
