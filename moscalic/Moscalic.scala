package moscalic

import java.io.{File, FileInputStream}

import com.sksamuel.scrimage.{Pixel, RGBColor, Image}

object Moscalic {

    val in = new FileInputStream(new File("/Users/ccoffey/Downloads/lambda-labs-logo_180x180.jpeg"))

    val image = Image.fromStream(in)

    // general algorithm is to
    // map each pixel to a color
    // compute average color of all pixels
    // this is the color of the image

    val

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



