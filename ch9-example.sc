import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._
import doodle.core.Point._
import doodle.core.PathElement._

/*
moveTo(Point)	Move the pen to Point without drawing.
lineTo(Point)	Draw a straight line to Point
curveTo(Point, Point, Point)	Draw a curve. The first two points specify control points and the last point is where the curve ends.
 */

// Creating Paths
val triangle =
  List(
    lineTo(cartesian(50, 100)),
    lineTo(cartesian(100, 0)),
    lineTo(cartesian(0, 0))
  )

val curve =
  List(curveTo(cartesian(50, 100), cartesian(100, 100), cartesian(150, 0)))

def style(image: Image): Image =
  image.
    lineWidth(6.0).
    lineColor(Color.royalBlue).
    fillColor(Color.skyBlue)

val openPaths =
  style(openPath(triangle) beside openPath(curve))

val closedPaths =
  style(closedPath(triangle) beside closedPath(curve))

val paths = openPaths above closedPaths

// List of Int
List(1, 2, 3)

// List of Image
List(Image.circle(10), Image.circle(20), Image.circle(30))

// List of Color
List(Color.paleGoldenrod, Color.paleGreen, Color.paleTurquoise)

def parametricLine(angle: Angle): Point =
  Point.cartesian(angle.cos * 50, angle.sin * 50)


/*
moveTo(polar(50, 0.degrees)) start at x = 50 y = 0
lineTo(polar(50, 120.degrees)) start at x = 50 move y 120 degrees for 50 pixels
lineTo(polar(50, 240.degrees)) start at x = 50 move y 240 degrees for 50 pixels

we use closed path that draws the last line
 */
val triangle =
  closedPath(List(
    moveTo(polar(50, 0.degrees)),
    lineTo(polar(50, 120.degrees)),
    lineTo(polar(50, 240.degrees))
  ))


val square =
  closedPath(List(
    moveTo(polar(50, 45.degrees)),  // go to upper right corner
    lineTo(polar(50, 135.degrees)), // draw up to upper left corner
    lineTo(polar(50, 225.degrees)), // draw up to lower left corner
    lineTo(polar(50, 315.degrees))  // draw up to lower right corner
  ))

val pentagon =
  closedPath((List(
    moveTo(polar(50, 72.degrees)),
    lineTo(polar(50, 144.degrees)),
    lineTo(polar(50, 216.degrees)),
    lineTo(polar(50, 288.degrees)),
    lineTo(polar(50, 360.degrees))
  )))

// Working with Lists
1 :: 2 :: 3 :: 4 :: Nil

def increment(list: List[Int]): List[Int] =
  list match {
    case Nil => Nil
    case hd :: tl => (hd + 1) :: increment(tl)
  }
// increment: (list: List[Int])List[Int]

increment(List(1, 2, 3))
// res2: List[Int] = List(2, 3, 4)

/*
Structural Recursion over a List
A List of elements of type A is:

  the empty list Nil; or
an element a of type A and a tail of type List[A]: a :: tail
The structural recursion skeleton for transforming list of type List[A] to some type B has shape

def doSomething[A,B](list: List[A]): B =
  list match {
    case Nil => ??? // Base case of type B here
    case hd :: tl => f(hd, doSomething(tl))
  }
*/

def ones(a: Int): List[Int] =
  a match {
    case 0 => Nil
    case n => 1 :: ones(n-1)
  }

ones(3)

def descending(a: Int): List[Int] =
  a match {
    case 0 => Nil
    case n => n :: descending(n-1)
  }

descending(3)

def ascending(a: Int): List[Int] = descending(a).reverse

ascending(3)

def fill[T](a : Int, item: T): List[T] =
  a match {
    case 0 => Nil
    case n => item :: fill(n-1, item)
  }

fill(3, "Hi")

// Write a method double that accepts a List[Int] and returns a list with each element doubled.

def double(items: List[Int]): List[Int] =
  items match {
    case Nil => Nil
    case head :: tail => head * 2 :: double(tail)
  }

double(1 :: 2 :: 4 :: 6 :: Nil)

// Write a method product that accepts a List[Int] and calculates the product of all the elements.

def product(items: List[Int]): Int = {
  def loop(items: List[Int], product: Int): Int =
    items match {
      case Nil => product
      case head :: tail => loop(tail, head * product)
    }

  loop(items, 1)
}

product(1 :: 2 :: 4 :: 6 :: Nil)

// Write a method contains that accepts a List[A] and an element of type A and returns true if the list contains the element and false otherwise.

def contains[T](heyStack: List[T], needle: T): Boolean =
  heyStack match {
    case Nil => false
    case head :: _ if head == needle => true
    case head :: tail if head != needle => contains(tail, needle)
  }

contains(List(1,2,3), 3)
contains(List("one", "two", "three"), "four")

def reverse[A](list: List[A]): List[A] = {
  def iter(list: List[A], reversed: List[A]): List[A] =
    list match {
      case Nil => reversed
      case hd :: tl => iter(tl, hd :: reversed)
    }

  iter(list, Nil)
}
// reverse: [A](list: List[A])List[A]

reverse(List(1, 2, 3))
// res36: List[Int] = List(3, 2, 1)

reverse(List("a", "b", "c"))
// res37: List[String] = List(c, b, a)

import Point._
import PathElement._

def polygon(sides: Int, size: Int, initialRotation: Angle): Image = {
  def iter(n: Int, rotation: Angle): List[PathElement] =
    n match {
      case 0 =>
        Nil
      case n =>
        LineTo(polar(size, rotation * n + initialRotation)) :: iter(n - 1, rotation)
    }
  closedPath(moveTo(polar(size, initialRotation)) :: iter(sides, 360.degrees / sides))
}

def style(img: Image): Image = {
  img.
    lineWidth(3.0).
    lineColor(Color.mediumVioletRed).
    fillColor(Color.paleVioletRed.fadeOut(0.5.normalized))
}

def makeShape(n: Int, increment: Int): Image =
  polygon(n+2, n * increment, 0.degrees)

def makeColor(n: Int, spin: Angle, start: Color): Color =
  start spin (spin * n)

val baseColor = Color.hsl(0.degrees, 0.7.normalized, 0.7.normalized)

def makeImage(n: Int): Image = {
  n match {
    case 0 =>
      Image.empty
    case n =>
      val shape = makeShape(n, 10)
      val color = makeColor(n, 30.degrees, baseColor)
      makeImage(n-1) on (shape fillColor color)
  }
}

val image = makeImage(15)

// Transforming Sequences
def increment(list: List[Int]): List[Int] =
  list match {
    case Nil => Nil
    case hd :: tl => (hd + 1) :: tl
  }
// increment: (list: List[Int])List[Int]

increment(List(1, 2, 3))
// res0: List[Int] = List(2, 2, 3)

def increment(list: List[Int]): List[Int] =
  list.map(x => x + 1)
// increment: (list: List[Int])List[Int]

increment(List(1, 2, 3))
// res1: List[Int] = List(2, 3, 4)

def fill[A](n: List[Int], a: A): List[A] =
  n.map(x => a)
// fill: [A](n: List[Int], a: A)List[A]

fill(List(1, 1, 1), "Hi")
// res3: List[String] = List(Hi, Hi, Hi)

0 until 10

0 until 10 by 2

(0 until 7).toList
// res7: List[Int] = List(0, 1, 2, 3, 4, 5, 6)

(0 until 3).map(x => x + 1).toList
// res8: List[Int] = List(1, 2, 3)

def fill[A](n: Int, a: A): List[A] =
  (0 until n).toList.map(x => a)
// fill: [A](n: Int, a: A)List[A]

fill(3, "Hi")

import scala.math.BigDecimal
BigDecimal(0.0) to 10.0 by 1.0

BigDecimal(10.0).doubleValue()
// res13: Double = 10.0

BigDecimal(10.0).intValue()
// res14: Int = 10

def ones(n: Int): List[Int] =
  (0 until n).toList.map(x => 1)
// ones: (n: Int)List[Int]

ones(3)
// res16: List[Int] = List(1, 1, 1)

def descending(n: Int): List[Int] =
  (n until 0 by -1).toList
// descending: (n: Int)List[Int]

descending(0)
// res19: List[Int] = List()

descending(3)
// res20: List[Int] = List(3, 2, 1)

def ascending(n: Int): List[Int] =
  (0 until n).toList.map(x => x + 1)
// ascending: (n: Int)List[Int]

ascending(0)
// res23: List[Int] = List()

ascending(3)
// res24: List[Int] = List(1, 2, 3)

def double(list: List[Int]): List[Int] =
  list map (x => x * 2)
// double: (list: List[Int])List[Int]

double(List(1, 2, 3))
// res27: List[Int] = List(2, 4, 6)

double(List(4, 9, 16))
// res28: List[Int] = List(8, 18, 32)

def polygon(sides: Int, size: Int, initialRotation: Angle): Image = {
  import Point._
  import PathElement._

  val step = (Angle.one / sides).toDegrees.toInt
  val path =
    (0 to 360 by step).toList.map{ deg =>
      lineTo(polar(size, initialRotation + deg.degrees))
    }

  closedPath(moveTo(polar(size, initialRotation)) :: path)
}

1 until 5
// res29: scala.collection.immutable.Range = Range 1 until 5

1 to 5
// res30: scala.collection.immutable.Range.Inclusive = Range 1 to 5

def ascending(n: Int): List[Int] =
  (1 to n).toList
// ascending: (n: Int)List[Int]

ascending(0)
// res33: List[Int] = List()

ascending(3)
// res34: List[Int] = List(1, 2, 3)

def star(sides: Int, skip: Int, radius: Double): Image = {
  import Point._
  import PathElement._

  val rotation = 360.degrees * skip / sides

  val start = moveTo(polar(radius, 0.degrees))
  val elements = (1 until sides).toList map { index =>
    val point = polar(radius, rotation * index)
    lineTo(point)
  }

  closedPath(start :: elements) lineWidth 2
}

def style(img: Image, hue: Angle): Image = {
  img.
    lineColor(Color.hsl(hue, 1.normalized, .25.normalized)).
    fillColor(Color.hsl(hue, 1.normalized, .75.normalized))
}

allBeside(
  (1 to 5).toList map { skip =>
    star(11, skip, 100)
  }
)

def allAbove(imgs: List[Image]): Image =
  imgs match {
    case Nil => Image.empty
    case hd :: tl => hd above allAbove(tl)
  }

allAbove((3 to 33 by 2).toList map { sides =>
  allBeside((1 to sides/2).toList map { skip =>
    style(star(sides, skip, 20), 360.degrees * skip / sides)
  })
})