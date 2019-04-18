import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

def boxes(color: Color): Image = {
  val box =
    Image.rectangle(40, 40).
      lineWidth(5.0).
      lineColor(color.spin(30.degrees)).
      fillColor(color)

  box beside box beside box beside box beside box
}

// Create boxes with different colors
boxes(Color.paleGoldenrod)
boxes(Color.lightSteelBlue)
boxes(Color.mistyRose)

/*
def methodName(param1: Param1Type, ...): ResultType =
  bodyExpression
 */

// Write a method square that accepts an Int argument and returns the Int square of it’s argument.

def square(x: Int): Int = {
  x * x
}

// Write a method halve that accepts a Double argument and returns the Double that is half of it’s argument.

def halve(a: Double): Double = {
  a / 2.0
}

// Method Semantics