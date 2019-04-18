import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

val aBox = Image.rectangle(20, 20).fillColor(Color.royalBlue)
val oneBox = aBox
val twoBoxes = aBox beside oneBox
val threeBoxes = aBox beside twoBoxes


// Or

def boxes(count: Int): Image =
  count match {
    case 0 => Image.empty
    case n => aBox beside boxes(n-1)
  }

"abcd" match {
  case "bcde" => 0
  case "cdef" => 1
  case "abcd" => 2
} // => 2

1 match {
  case 0 => "zero"
  case 1 => "one"
  case 1 => "two"
} // => "one"

1 match {
  case n => n + 1
  case 1 => 1000
} // => 2


1 match {
  case a => a
  case b => b + 1
  case c => c * 2
} // => 1

// Match Error
//2 match {
//  case 0 => "zero"
//  case 1 => "one"
//}

/*
Structural Recursion over Natural Numbers Pattern
The general pattern for structural recursion over the natural numbers is

def name(count: Int): Result =
  count match {
    case 0 => resultBase
    case n => resultUnit add name(n-1)
  }
 */

def cross(count: Int): Image = {
  val unit = Image.circle(20)
  count match {
    case 0 => unit
    case n => unit beside (unit above cross(n-1) above unit) beside unit
  }
}

def chessboard(count: Int): Image = {
  val unit = {
    (square(10) beside square(10)) below (square(10) beside square(10))
  }
  count match {
    case 0 => unit
    case n =>
      val rest = chessboard(n - 1)
      (rest beside rest) below (rest beside rest)
  }
}

def sierpinski(count: Int): Image = {
  val unit = {
    (triangle(20, 20) beside triangle(20, 20)) below triangle(20, 20)
  }
  count match {
    case 1 => unit
    case n =>
      val rest = sierpinski(n-1)
      (rest beside rest) below rest
  }
}


// Given a natural number, returns that number
// Examples:
//   identity(0) == 0
//   identity(3) == 3
def identity(x: Int): Int =
  x match {
    case 0 => 0
    case n => 1 + identity(n-1)
  }

/*
identity(3) =>
3 match {
  1 + 2 match {
    1 + 1 match {
      1 + 0
    }
  }
} =>

1 + 1 + 1 => 3
 */

identity(3)

//  Auxiliary Parameters
def growingBoxes(count: Int): Image =
  count match {
    case 0 => Image.empty
    case n => growingBoxes(n-1) beside Image.rectangle(n*10, n*10)
  }

def gradientBoxes(n: Int, color: Color): Image =
  n match {
    case 0 => Image.empty
    case n => aBox.fillColor(color) beside gradientBoxes(n-1, color.spin(15.degrees))
  }

def concentricCircles(n: Int): Image =
  n match {
    case 0 => Image.Empty
    case n => concentricCircles(n-1) on Circle(n*5)
  }

def circle(size: Int, color: Color): Image =
  Image.circle(size).lineWidth(3.0).lineColor(color)


def fadeCircles(n: Int, size: Int, color: Color): Image =
  n match {
    case 0 => Image.empty
    case n => circle(size, color) on fadeCircles(n-1, size+7, color.fadeOutBy(0.05.normalized))
  }

def gradientCircles(n: Int, size: Int, color: Color): Image =
  n match {
    case 0 => Image.empty
    case n => circle(size, color) on gradientCircles(n-1, size+7, color.spin(15.degrees))
  }

// Nested Methods

// Unit is not created on each loop now
def cross(count: Int): Image = {
  val unit = {
    println("Creating unit")
    Image.circle(20)
  }
  def loop(count: Int): Image = {
    count match {
      case 0 => unit
      case n => unit beside (unit above loop(n-1) above unit) beside unit
    }
  }

  loop(count)
}

def chessboard(count: Int): Image = {
  val blackSquare = Image.rectangle(30, 30) fillColor Color.black
  val redSquare   = Image.rectangle(30, 30) fillColor Color.red
  val base =
    (redSquare   beside blackSquare) above (blackSquare beside redSquare)
  def loop(count: Int): Image =
    count match {
      case 0 => base
      case n =>
        val unit = loop(n-1)
        (unit beside unit) above (unit beside unit)
    }

  loop(count)
}

def boxes(count: Int): Image = {
  val aBox = Image.rectangle(20, 20).fillColor(Color.royalBlue)
  def loop(count: Int): Image =
    count match {
      case 0 => Image.empty
      case n => aBox beside loop(n-1)
    }

  loop(count)
}