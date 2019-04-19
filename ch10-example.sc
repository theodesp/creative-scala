import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._
import doodle.turtle._
import doodle.turtle.Instruction._

forward(10)
// res0: doodle.turtle.Instruction = Forward(10.0)

turn(5.degrees)

val instructions =
  List(forward(10), turn(90.degrees),
    forward(10), turn(90.degrees),
    forward(10), turn(90.degrees),
    forward(10))

val path = Turtle.draw(instructions)

def polygon(sides: Int, sideLength: Double): Image = {
  val rotation = Angle.one / sides
  def iter(n: Int): List[Instruction] =
    n match {
      case 0 => Nil
      case n => turn(rotation) :: forward(sideLength) :: iter(n-1)
    }

  Turtle.draw(iter(sides))
}

def squareSpiral(steps: Int, distance: Double, angle: Angle, increment: Double): Image = {
  def iter(n: Int, distance: Double): List[Instruction] = {
    n match {
      case 0 => Nil
      case n => forward(distance) :: turn(angle) :: iter(steps-1, distance + increment)
    }
  }

  Turtle.draw(iter(steps, distance))
}

import doodle.turtle._
import doodle.turtle.Instruction._

val y = Turtle.draw(List(
  forward(100),
  branch(turn(45.degrees), forward(100)),
  branch(turn(-45.degrees), forward(100))))

val stepSize = 10
// stepSize: Int = 10

def rule(i: Instruction): List[Instruction] =
  i match {
    case Forward(_) => List(forward(stepSize), forward(stepSize))
    case NoOp =>
      List(branch(turn(45.degrees), forward(stepSize), noop),
        branch(turn(-45.degrees), forward(stepSize), noop))
    case other => List(other)
  }


// Flatmap signature

// List[A] flatMap (A => List[B]) = List[B]
//There are two points to this:
//
//  recognising how to use flatMap; and
//remembering how to use type variables.
def double[A](in: List[A]): List[A] =
  in.flatMap { x => List(x, x) }

def nothing[A](in: List[A]): List[A] =
  in.flatMap { x => List.empty }

def rewrite(instructions: List[Instruction], rule: Instruction => List[Instruction]): List[Instruction] =
  instructions.flatMap { i =>
    i match {
      case Branch(i) =>
        List(branch(rewrite(i, rule):_*))
      case other =>
        rule(other)
    }
  }

def iterate(steps: Int,
            seed: List[Instruction],
            rule: Instruction => List[Instruction]): List[Instruction] =
  steps match {
    case 0 => seed
    case n => iterate(n - 1, rewrite(seed, rule), rule)
  }

def polygon(sides: Int, sideLength: Double): Image = {
  val rotation = Angle.one / sides

  Turtle.draw((1 to sides).toList.flatMap { n =>
    List(turn(rotation), forward(sideLength))
  })
}

def squareSpiral(steps: Int, distance: Double, angle: Angle, increment: Double): Image = {
  Turtle.draw((1 to steps).toList.flatMap { n =>
    List(forward(distance + (n * increment)), turn(angle))
  })
}