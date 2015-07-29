package moscalic

import java.io.{File, FileInputStream}

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.{Pixel, RGBColor, Image}

object Moscalic {

    def main(a: Array[String]) {
        val image = Image.fromFile(new File("/Users/ccoffey/Downloads/lambda-labs-logo_180x180.jpeg"))

        // general algorithm is to
        // map each pixel to a color
        // compute average color of all pixels
        // tag the image with its average color

        // to build the mosaic, get a collection of images
        // find their average colors
        // sort by average color
        // Divide the target image into

        val newImg = image.scaleTo(2 * image.width, 2 * image.height)
        newImg.output("big.jpeg")(JpegWriter())
    }
}

trait ColorAverage[A] {
    def apply(l: A, r: A): RGBColor
}

object ColorAverage {
    def apply[A](l: A, r: A)(implicit avg: ColorAverage[A]): RGBColor = avg(l, r)

    implicit object PixelColorAvg extends ColorAverage[Pixel] {
        def apply(l: Pixel, r: Pixel) =
            RGBColor((l.toColor.red + r.toColor.red)/2, (l.toColor.green+ r.toColor.green)/ 2, (l.toColor.blue + r.toColor.blue) / 2)
    }


}



