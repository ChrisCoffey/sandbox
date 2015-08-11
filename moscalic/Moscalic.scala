package moscalic

import java.awt.image.BufferedImage
import java.io.{File, FileInputStream}
import javax.imageio.ImageWriter
import scala.collection.mutable.ArrayBuffer

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage._

object Moscalic {

    trait Distance[T] {
       def distance(r: T): Float
    }

    implicit def colorDistance(c: RGBColor)= new Distance[RGBColor] {
        def distance(r: RGBColor) = {
            val pr = Math.abs(c.red - r.red) / 255f
            val pg = Math.abs(c.green - r.green) / 255f
            val pb = Math.abs(c.blue - r.blue) / 255f

            ((pr + pg + pb) / 3) * 100
        }
    }

    trait Comprable[T] {
        def lt(r: T): Boolean
        def gt(r: T): Boolean
        def ~~(r: T): Boolean = !(this.lt(r) && this.gt(r))
    }

    val zero = RGBColor(0, 0, 0)
    implicit def ColorComprable(c: RGBColor): Comprable[RGBColor] = new Comprable[RGBColor] {

        def lt (r: RGBColor): Boolean = {
            (c distance zero) < (r distance zero)
        }

        def gt(r: RGBColor): Boolean = {
            (c distance zero) > (r distance zero)
        }
    }

    implicit object ColorOrdering extends Ordering[RGBColor]{
        override def compare(x: RGBColor, y: RGBColor): Int = x.distance(y).toInt
    }

    def main(a: Array[String]) {
        val images = new File("/Users/ccoffey/workspace/open_source/scrimage/examples/composite")
            .listFiles()
            .map(f => Image.fromFile(f))
            .map(i => (averageColor(i).distance(zero), i))
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

        val imageGrid = ArrayBuffer.fill(i2.width)(ArrayBuffer.fill(i2.height)(i2))
        for {
            x <- 0 until i2.width
            y <- 0 until i2.height
        }{
            imageGrid(x)(y) = imageForColor(i2.pixel(x, y).toColor.distance(zero), images).cover(50, 50)
        }

        val buff = new BufferedImage(i2.width * 50, i2.height * 50, BufferedImage.TYPE_INT_RGB)
        for {
            x <- imageGrid.indices.par
            y <- imageGrid(x).indices
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

    def imageForColor(c: Float, ls: List[(Float, Image)]): Image =
        ls match {
            case h :: ha :: Nil => if(Math.abs(h._1 - c) > Math.abs(ha._1 - c)) ha._2 else h._2
            case _ =>
                ls(ls.length/ 2) match {
                    case (x, img) if x < c => imageForColor(c, ls.drop(ls.length / 2))
                    case (x, img) if x > c => imageForColor(c, ls.take(ls.length / 2))
                    case (x, img) if x == c => img
                }
        }
}





