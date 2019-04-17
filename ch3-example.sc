import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

Image.circle(10)
circle(10)

// A circle beside a rectangle
// (circle(10) beside rectangle(10, 20)).draw

// rectangle(100, 50).draw
// circle(10).draw
// triangle(60, 40).draw

//Create circles that are 1, 10, and 100 units wide. Now draw them!

//for (i <- Seq(1, 10, 100)) {
//  circle(i).draw
//}

// What is the type of a circle? A rectangle? A triangle?
// Image

//What’s the type of drawing an image? What does this mean?

//:type circle(10).draw
//// Unit

//(circle(20) beside circle(20) beside circle(20) on circle(60)).draw

/*
Image fillColor Color: Fills the image with the specified colour.
Image lineColor Color: Outlines the image with the specified colour..
Image lineWidth Int: Outlines the image with the specified stroke width..
 */

// Evil Eye

val outer = circle(60) fillColor Color.darkBlue
val mid = circle(30) fillColor Color.white
val inner = circle(20) fillColor Color.cornflowerBlue
var core = circle(10) fillColor Color.black

(core on inner on mid on outer).draw

((triangle(40, 40)
  lineWidth 6.0
  lineColor Color.darkSlateBlue
  fillColor (Color.darkSlateBlue lighten 0.3.normalized saturate 0.2.normalized spin 10.degrees)) above
  ((triangle(40, 40)
    lineWidth 6.0
    lineColor (Color.darkSlateBlue spin (-30.degrees))
    fillColor (Color.darkSlateBlue lighten 0.3.normalized saturate 0.2.normalized spin (-20.degrees))) beside
    (triangle(40, 40)
      lineWidth 6.0
      lineColor (Color.darkSlateBlue spin (30.degrees))
      fillColor (Color.darkSlateBlue lighten 0.3.normalized saturate 0.2.normalized spin (40.degrees))))).draw


(
  ( circle(10) fillColor Color.red ) on
    ( circle(20) fillColor Color.white ) on
    ( circle(30) fillColor Color.red lineWidth 2 ) above
    ( rectangle(6, 20) above rectangle(20, 6) fillColor Color.brown ) above
    ( rectangle(80, 25) lineWidth 0 fillColor Color.green )
  ).draw





