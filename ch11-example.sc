import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._
import doodle.random._

def randomAngle: Angle =
  math.random.turns
// randomAngle: doodle.core.Angle

randomAngle
// res2: doodle.core.Angle = Angle(5.203840701881509)

randomAngle
// res3: doodle.core.Angle = Angle(5.030378724914573)
val randomDouble = Random.double

randomDouble.run

// Random.always(value)	Creates a Random that always produces the given value.
// Random.double Creates a Random that generates a Double uniformly distributed between 0.0 and 1.0.
// Random.int Creates a Random that generates an Int uniformly distributed across the entire range.
// Random.natural(limit)	Creates a Random that generates a Int uniformly distributed in the range greater than or equal to 0 and less than 1.
// Random.oneOf(value, ...)	Creates a Random that generates one of the given values with equal chance.

val randomAngle: Random[Angle] =
  Random.double.map(x => x.turns)

randomAngle.run

def randomColor(s: Normalized, l: Normalized): Random[Color] =
  randomAngle map (hue => Color.hsl(hue, s, l))

def randomCircle(r: Double, color: Random[Color]): Random[Image] =
  color map (fill => Image.circle(r) fillColor fill)

def concentricCircles(count: Int, size: Int, color: Color): Image =
  count match {
    case 0 => Image.empty
    case n =>
      Image.circle(size).fillColor(color) on concentricCircles(n-1, size + 5, color.spin(15.degrees))
  }

val randomPastel = randomColor(0.7.normalized, 0.7.normalized)

def randomConcentricCircles(count: Int, size: Int): Random[Image] =
  count match {
    case 0 => Random.always(Image.empty)
    case n =>
      randomCircle(size, randomPastel) flatMap { circle =>
        randomConcentricCircles(n-1, size + 5) map { circles =>
          circle on circles
        }
      }
  }

def coloredRectangle(color: Color): Image =
  rectangle(20, 20) fillColor color

//def randomColorBoxes(count: Int): Random[Image] =
//  count match {
//    case 0 => randomColor map { c => coloredRectangle(c) }
//    case n =>
//      val box = randomColor map { c => coloredRectangle(c) }
//      val boxes = randomColorBoxes(n-1)
//      box flatMap { b =>
//        boxes map { bs => b beside bs }
//      }
//  }

def nextColor(color: Color): Random[Color] = {
  val spin = Random.normal(15.0, 10.0)
  spin map { s => color.spin(s.degrees) }
}

def coloredRectangle(color: Color, size: Int): Image =
  rectangle(size, size).
    lineWidth(5.0).
    lineColor(color.spin(30.degrees)).
    fillColor(color)


def randomGradientBoxes(count: Int, color: Color): Random[Image] =
  count match {
    case 0 => Random.always(Image.empty)
    case n =>
      val box = coloredRectangle(color, 20)
      val boxes = nextColor(color) flatMap { c => randomGradientBoxes(n-1, c) }
      boxes map { b => box beside b }
  }

randomGradientBoxes(2, Color.red)

def randomPoint(): Random[Point] =
  Random.double flatMap { x =>
    Random.double map { y =>
      Point.Cartesian(x, y)
    }
  }

randomPoint().run

val start = Random.always(Point.zero)

def step(current: Point): Random[Point] = {
  val drift = Point(current.x + 10, current.y)
  val noise =
    Random.normal(0.0, 5.0) flatMap { x =>
      Random.normal(0.0, 5.0) map { y =>
        Vec(x, y)
      }
    }

  noise.map(vec => drift + vec)
}

def render(point: Point): Image = {
  val length = (point - Point.zero).length
  val sides = (length / 20).toInt + 3
  val hue = (length / 200).turns
  val color = Color.hsl(hue, 0.7.normalized, 0.5.normalized)
  Image.
    star(sides, 5, 3, 0.degrees).
    noFill.
    lineColor(color).
    at(point.toVec)
}

def walk(steps: Int): Random[Image] = {
  def loop(count: Int, current: Point, image: Image): Random[Image] = {
    count match {
      case 0 => Random.always(image on render(current))
      case n =>
        val next = step(current)
        next flatMap { pt =>
          loop(count - 1, pt, image on render(current))
        }
    }
  }

  start flatMap { pt => loop(steps, pt, Image.empty) }
}

def particleSystem(particles: Int, steps: Int): Random[Image] = {
  particles match {
    case 0 => Random.always(Image.empty)
    case n => walk(steps) flatMap { img1 =>
      particleSystem(n-1, steps) map { img2 =>
        img1 on img2
      }
    }
  }
}

def randomConcentricCircles(count: Int, size: Int): Random[Image] =
  count match {
    case 0 => Random.always(Image.empty)
    case n =>
      for {
        circle  <- randomCircle(size, randomPastel)
        circles <- randomConcentricCircles(n-1, size + 5)
      } yield circle on circles
  }

/*
for {
  x <- a
  y <- b
  z <- c
} yield e


translates to:

a.flatMap(x => b.flatMap(y => c.map(z => e)))
 */

def makePoint(x: Random[Double], y: Random[Double]): Random[Point] =
  for {
    theX <- x
    theY <- y
  } yield Point.cartesian(theX, theY)

val normal = Random.normal(50, 15)
val normal2D = makePoint(normal, normal)

val data = (1 to 1000).toList.map(_ => normal2D)

def point(loc: Point): Image =
  circle(2).fillColor(Color.cadetBlue.alpha(0.3.normalized)).noLine.at(loc.toVec)

val points = data.map(r => r.map(point _))

def allOn(points: List[Random[Image]]): Random[Image] =
  points match {
    case Nil => Random.always(Image.empty)
    case img :: imgs =>
      for {
        i  <- img
        is <- allOn(imgs)
      } yield (i on is)
  }

val plot = allOn(points)

def perturb(point: Point): Random[Point] =
  for {
    x <- Random.normal(0, 10)
    y <- Random.normal(0, 10)
  } yield Point.cartesian(point.x + x, point.y + y)

def rose(k: Int): Angle => Point =
  (angle: Angle) => {
    Point.cartesian((angle * k).cos * angle.cos, (angle * k).cos * angle.sin)
  }

def perturbedRose(k: Int): Angle => Random[Point] =
  rose(k) andThen perturb


object ParametricNoise {
  def rose(k: Int): Angle => Point =
    (angle: Angle) => {
      Point.cartesian((angle * k).cos * angle.cos, (angle * k).cos * angle.sin)
    }

  def scale(factor: Double): Point => Point =
    (pt: Point) => {
      Point.polar(pt.r * factor, pt.angle)
    }

  def perturb(point: Point): Random[Point] =
    for {
      x <- Random.normal(0, 10)
      y <- Random.normal(0, 10)
    } yield Point.cartesian(point.x + x, point.y + y)

  def smoke(r: Normalized): Random[Image] = {
    val alpha = Random.normal(0.5, 0.1) map (a => a.normalized)
    val hue = Random.double.map(h => (h * 0.1).turns)
    val saturation = Random.double.map(s => (s * 0.8).normalized)
    val lightness = Random.normal(0.4, 0.1) map (a => a.normalized)
    val color =
      for {
        h <- hue
        s <- saturation
        l <- lightness
        a <- alpha
      } yield Color.hsla(h, s, l, a)
    val c = Random.normal(5, 5) map (r => circle(r))

    for {
      circle <- c
      line   <- color
    } yield circle.lineColor(line).noFill
  }

  def point(
             position: Angle => Point,
             scale: Point => Point,
             perturb: Point => Random[Point],
             image: Normalized => Random[Image],
             rotation: Angle
           ): Angle => Random[Image] = {
    (angle: Angle) => {
      val pt = position(angle)
      val scaledPt = scale(pt)
      val perturbed = perturb(scaledPt)

      val r = pt.r.normalized
      val img = image(r)

      for {
        i  <- img
        pt <- perturbed
      } yield (i at pt.toVec.rotate(rotation))
    }
  }

  def iterate(step: Angle): (Angle => Random[Image]) => Random[Image] = {
    (point: Angle => Random[Image]) => {
      def iter(angle: Angle): Random[Image] = {
        if(angle > Angle.one)
          Random.always(Image.empty)
        else
          for {
            p  <- point(angle)
            ps <- iter(angle + step)
          } yield (p on ps)
      }

      iter(Angle.zero)
    }
  }

  val image: Random[Image] = {
    val pts =
      for(i <- 28 to 360 by 39) yield {
        iterate(1.degrees){
          point(
            rose(5),
            scale(i),
            perturb _,
            smoke _,
            i.degrees
          )
        }
      }
    val picture = pts.foldLeft(Random.always(Image.empty)){ (accum, img) =>
      for {
        a <- accum
        i <- img
      } yield (a on i)
    }
    val background = (rectangle(650, 650) fillColor Color.black)

    picture map { _ on background }
  }
}