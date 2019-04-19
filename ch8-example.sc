// Horticulture and Higher-order Functions
import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._


def parametricCircle(angle: Angle): Point =
  ???

/*
Point.cartesian(Double, Double): Constructs a Point using the cartesian representation.
Point.polar(Double, Angle) Point(Double, Angle): Constructs a Point using the polar representation.
 */

val dot = Image.circle(5).lineWidth(3).lineColor(Color.crimson)
val squareDots =
  dot.at(0, 0).
    on(dot.at(0, 100)).
    on(dot.at(100, 100)).
    on(dot.at(100, 0))


// Geometry
val polar = Point.polar(1.0, 45.degrees)
// polar: doodle.core.Point = Polar(1.0,Angle(0.7853981633974483))

val cartesian = Point.cartesian((45.degrees.cos) * 1.0, (45.degrees.sin) * 1.0)
// cartesian: doodle.core.Point = Cartesian(0.7071067811865476,0.7071067811865475)

// They are the same
polar.toCartesian == cartesian
// res2: Boolean = true

cartesian.toPolar == polar
// res3: Boolean = true

def parametricCircle(angle: Angle): Point =
  Point.cartesian(angle.cos * 200, angle.sin * 200)

def sample(start: Angle, samples: Int): Image = {
  // Angle.one is one complete turn. I.e. 360 degrees
  val step = Angle.one / samples
  val dot = triangle(10, 10)
  def loop(count: Int): Image = {
    val angle = step * count
    count match {
      case 0 => Image.empty
      case n =>
        dot.at(parametricCircle(angle).toVec) on loop(n - 1)
    }
  }

  loop(samples)
}

// Parametric equation for rose with k = 7
def rose(angle: Angle) =
  Point.polar((angle * 7).cos * 200, angle)

(x: Int) => x + 42
// res4: Int => Int = $$Lambda$4898/1421663968@a1c1fff
// We can apply the function to an argument in the usual way.

val add42 = (x: Int) => x + 42
// add42: Int => Int = $$Lambda$4899/419472549@49129763

add42(0)
// res5: Int = 42

val roseFn = (angle: Angle) =>
  Point.cartesian((angle * 7).cos * angle.cos, (angle * 7).cos * angle.sin)

def concentricShapes(count: Int, singleShape: Int => Image): Image =
  count match {
    case 0 => Image.empty
    case n => singleShape(n) on concentricShapes(n-1, singleShape)
  }

def outlinedCircle(n: Int) =
  Image.circle(n * 10)


def circleOrSquare(n: Int) =
  if(n % 2 == 0) Image.rectangle(n*20, n*20) else Image.circle(n*10)

(concentricShapes(10, outlinedCircle) beside concentricShapes(10, circleOrSquare))

def concentricShapes(count: Int, singleShape: Int => Image): Image =
  count match {
    case 0 => Image.empty
    case n => singleShape(n) on concentricShapes(n-1, singleShape)
  }

def rainbowCircle(n: Int) = {
  val color = Color.blue desaturate 0.5.normalized spin (n * 30).degrees
  val shape = Image.circle(50 + n*12)
  shape lineWidth 10 lineColor color
}

def fadingTriangle(n: Int) = {
  val color = Color.blue fadeOut (1 - n / 20.0).normalized
  val shape = Image.triangle(100 + n*24, 100 + n*24)
  shape lineWidth 10 lineColor color
}

def rainbowSquare(n: Int) = {
  val color = Color.blue desaturate 0.5.normalized spin (n * 30).degrees
  val shape = Image.rectangle(100 + n*24, 100 + n*24)
  shape lineWidth 10 lineColor color
}

val answer =
  (concentricShapes(10, rainbowCircle) beside
    concentricShapes(10, fadingTriangle) beside
    concentricShapes(10, rainbowSquare))

def concentricShapes(count: Int, singleShape: Int => Image): Image =
  count match {
    case 0 => Image.empty
    case n => singleShape(n) on concentricShapes(n-1, singleShape)
  }
// concentricShapes: (count: Int, singleShape: Int => doodle.core.Image)doodle.core.Image

def colored(shape: Int => Image, color: Int => Color): Int => Image =
  (n: Int) =>
    shape(n) lineWidth 10 lineColor color(n)
// colored: (shape: Int => doodle.core.Image, color: Int => doodle.core.Color)Int => doodle.core.Image

def fading(n: Int): Color =
  Color.blue fadeOut (1 - n / 20.0).normalized
// fading: (n: Int)doodle.core.Color

def spinning(n: Int): Color =
  Color.blue desaturate 0.5.normalized spin (n * 30).degrees
// spinning: (n: Int)doodle.core.Color

def size(n: Int): Double =
  50 + 12 * n
// size: (n: Int)Double

def circle(n: Int): Image =
  Circle(size(n))
// circle: (n: Int)doodle.core.Image

def square(n: Int): Image =
  Image.rectangle(2*size(n), 2*size(n))
// square: (n: Int)doodle.core.Image

def triangle(n: Int): Image =
  Image.triangle(2*size(n), 2*size(n))
// triangle: (n: Int)doodle.core.Image

val answer =
  (concentricShapes(10, colored(circle, spinning)) beside
    concentricShapes(10, colored(triangle, fading)) beside
    concentricShapes(10, colored(square, spinning)))


def sample(start: Angle, samples: Int, empty: Image, combine: (Angle, Image) => Image): Image = {
  // Angle.one is one complete turn. I.e. 360 degrees
  val step = Angle.one / samples
  def loop(count: Int): Image = {
    val angle = step * count
    count match {
      case 0 => empty
      case n => combine(angle, loop(n - 1))
    }
  }

  loop(samples)
}

def locate(scale: Point => Point, point: Angle => Point): Angle => Point =
  (angle: Angle) => scale(point(angle))

def scale(factor: Double): Point => Point =
  (pt: Point) => {
    Point.polar(pt.r * factor, pt.angle)
  }

// Rose on circle
val flower = {
  sample(0.degrees, 200, locate(scale(200), rose _)) on
    sample(0.degrees, 40, locate(scale(150), parametricCircle _))
}