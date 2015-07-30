package moscalic

import java.awt.image.BufferedImage
import java.io.{File, FileInputStream}
import javax.imageio.ImageWriter
import scala.collection.mutable.ArrayBuffer

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage._

object Moscalic {

    def main(a: Array[String]) {
        val images = new File("/Users/ccoffey/workspace/open_source/scrimage/examples/composite")
            .listFiles()
            .map(f => Image.fromFile(f))
            .map(i => (averageColor(i).toInt, i))
            .sortBy(_._1)
            .toList

        // general algorithm is to
        // map each pixel to a color
        // compute average color of all pixels
        // tag the image with its average color

        // to build the mosaic, get a collection of images
        // find their average colors
        // sort by average color
        // Divide the target image into

        val i2 =  images.head._2.cover(100, 100)
        val ac = averageColor(images.head._2)
        val ac2 = averageColor(i2)

        println(ac)
        println(ac2)
        println(averageColor(imageForColor(ac, images)))

        val imageGrid = ArrayBuffer.fill(i2.width)(ArrayBuffer.fill(i2.height)(i2))
        for {
            x <- 0 until i2.width
            y <- 0 until i2.height
        }{
            imageGrid(x)(y) = imageForColor(i2.pixel(x, y).toColor, images).cover(150, 150)
        }

        val buff = new BufferedImage(i2.width * 50, i2.height * 50, BufferedImage.TYPE_INT_RGB)
        for {
            x <- imageGrid.indices.filter(_ % 2 == 0)
            y <- imageGrid(x).indices.filter(_ % 2 == 0)
            m <- 0 until 50
            n <- 0 until 50
            i = (math.max(x - 1, 0) * 50) + m
            j = (math.max(y - 1, 0) * 50) + n
        } {
            buff.setRGB(i, j, imageGrid(x)(y).pixel(m, n).toInt)
        }

        Image.fromAwt(buff, 1).output("perhaps.png")
    }


    def averageColor(i: Image): RGBColor = {
        val colorAgg = i.pixels.map(_.toColor.toRGB)
            .foldLeft((0, 0, 0))((acc, p) => (acc._1 + p.red, acc._2 + p.green, acc._3 + p.blue))
        RGBColor(colorAgg._1 / i.pixels.length, colorAgg._2 / i.pixels.length, colorAgg._3 / i.pixels.length)
    }

    def imageForColor(c: RGBColor, ls: List[(Int, Image)]): Image =
        ls match {
            case h :: Nil => h._2
            case _ => {
                val i = c.toInt
                ls(ls.length/ 2) match {
                    case (x, img) if x < i => imageForColor(c, ls.drop(ls.length / 2))
                    case (x, img) if x > i => imageForColor(c, ls.take(ls.length / 2))
                    case (x, img) if x == i => img
                }
            }
        }
}





