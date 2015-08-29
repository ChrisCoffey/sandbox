package moscalic

import java.awt.image.BufferedImage
import java.io.{File, FileInputStream}
import javax.imageio.ImageWriter
import scala.collection.mutable.ArrayBuffer

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage._

object Moscalic {

    val pixelImageSize = 50

    def main(a: Array[String]) {
        val images = new File("/Users/ccoffey/workspace/open_source/scrimage/examples/filters")
            .listFiles()
            .map(f => Image.fromFile(f))
            .toList

        val i2 =  images.last.cover(250, 250)

        val compressedImages = images
            .map(_.cover(pixelImageSize, pixelImageSize))
            .map(i => (averageColor(i), i))

        val imageGrid = ArrayBuffer.fill(i2.width)(ArrayBuffer.fill(i2.height)(i2))
        for {
            x <- (0 until i2.width).par
            y <- 0 until i2.height
        }{
            imageGrid(x)(y) =
                imageForColor(LabColor.toLabSpace(XYZColor.toXyz(i2.pixel(x, y).toColor), XYZColor.D65WhitePoint), compressedImages)
        }

        val buff = new BufferedImage(i2.width * pixelImageSize, i2.height * pixelImageSize, BufferedImage.TYPE_INT_RGB)
        imageGrid.indices.par.foreach{ x =>
           for {
                y <- imageGrid(x).indices
                m <- 0 until pixelImageSize
                n <- 0 until pixelImageSize
                i = (math.max(x - 1, 0) * pixelImageSize) + m
                j = (math.max(y - 1, 0) * pixelImageSize) + n
            } {
               try{
                   buff.setRGB(i, j, imageGrid(x)(y).pixel(m, n).toInt)
               }catch {
                   case e: Exception =>
                       //todo understand where I'm crossing the boundaries here
                    println(s"i=$i  j=$j  x=$x  y=$y m=$m n=$n")
               }
            }
        }

        Image.fromAwt(buff, 1).output("perhaps.png")
    }

    def averageColor(i: Image): LabColor = {
        val colorAgg = i.pixels.map(_.toColor.toRGB)
            .foldLeft((0, 0, 0))((acc, p) => (acc._1 + p.red, acc._2 + p.green, acc._3 + p.blue))
        val rgb = RGBColor(colorAgg._1 / i.pixels.length, colorAgg._2 / i.pixels.length, colorAgg._3 / i.pixels.length)
        LabColor.toLabSpace(XYZColor.toXyz(rgb), XYZColor.D65WhitePoint)
    }

    case class XYZColor(x: Double, y: Double, z: Double)

    object XYZColor {
            //An Observer 2°, Illuminant D65 CIE-31 XYZ conversion
        def toXyz(rgb: RGBColor): XYZColor = {
            val gammaCompressionPt =  0.04045d
            def transform(a: Double): Double = {
                val aa = a / 255
                (if(aa > gammaCompressionPt)
                    math.pow(((aa + 0.055) / 1.055), 2.4)
                else
                    aa / 12.92) * 100
            }

            val r = transform(rgb.red.toDouble)
            val g = transform(rgb.green.toDouble)
            val b = transform(rgb.blue.toDouble)

            val x = r * 0.4214 + g * 0.3576 + b * 0.1805
            val y = r * 0.2126 + g * 0.7152 + b * 0.0722
            val z = r * 0.0193 + g * 0.1192 + b * 0.9505

            XYZColor(x, y, z)
        }

        val D65WhitePoint = XYZColor(0.95047, 1.0000, 1.08883)
    }

    case class LabColor(L: Double, a: Double, b: Double)
    object LabColor {
        private val coef1 = math.pow(6d/29, 3)
        private val coef2 = math.pow(29d/6, 2)
        private val oneThird = 1d/3
        private val fourOver29 = 4d/29
        private val prod = oneThird * coef2

        def toLabSpace(xyz: XYZColor, whitePoint: XYZColor) = {
            def f(t: Double) =
                if(t > coef1)
                    math.pow(t, oneThird)
                else
                    (prod * 29 * t) + fourOver29


            val fOfY = f(xyz.y / whitePoint.y)
            val L = 116 * fOfY
            val a = 500 * (f(xyz.x / whitePoint.x) - fOfY)
            val b = 200 * (fOfY - f(xyz.z - whitePoint.z))

            LabColor(L, a, b)
        }


        def deltaE(l1: LabColor, l2: LabColor) =
            math.sqrt( math.pow( l2.L - l1.L , 2) + math.pow(l2.a - l1.a, 2) + math.pow(l2.a - l1.a, 2) )

    }

    private var memo = Map[LabColor, Image]()
    def imageForColor(c: LabColor, ls: List[(LabColor, Image)]): Image = {
        if(memo.contains(c))
            memo(c)
        else {
           val r =  ls.foldLeft((1000d, c, Image.filled(1, 1, Color.Black))){(deltaE, color) =>
                val d = LabColor.deltaE(c, color._1)
                if(d < deltaE._1) (d, color._1,  color._2)
                else deltaE
            }

            val t = (r._2, r._3)
            memo = memo.+(t)
            r._3
        }

    }

}





