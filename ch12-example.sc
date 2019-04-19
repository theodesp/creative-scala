import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._
import doodle.turtle.Instruction

// An algebraic data type is built from two components: - logical ors; and - logical ands.

/*
A PathElement is a sum type, as it is: - a MoveTo; or - a LineTo; or - a CurveTo.

A MoveTo is a product type that holds a single point (where to move to).

A LineTo is a product type that holds a single point (the end point of the line).

A CurveTo is a product type that holds three points: two control points and the end point of the line.
 */

/*
An Instruction is: - a Forward; or - a Turn; or - a Branch; or - a NoOp

Therefore Instruction is a sum type. Forward, Turn, and Branch are all product types.

A Forward holds a distance, which is a Double.

A Turn holds an angle, which is an Angle.

A Branch holds a List[Instruction]—therefore the Instruction type is defined in terms of itself, just like List.

A NoOp holds no data.
 */

// If A is a B or C write

sealed abstract class A extends Product with Serializable
// defined class A

final case class B() extends A
// defined class B

final case class C() extends A
// defined class C

//If A has a B and C, write
//final case class A(b: B, c: C)

sealed abstract class PathElement extends Product with Serializable
final case class MoveTo(to: Point) extends PathElement
final case class LineTo(to: Point) extends PathElement
final case class CurveTo(cp1: Point, cp2: Point, to: Point) extends PathElement

//Define your own algebraic data type to represent Instruction.

sealed abstract class Instruction extends Product with Serializable
// defined class Instruction

final case class Forward(distance: Double) extends Instruction
// defined class Forward

final case class Turn(angle: Angle) extends Instruction
// defined class Turn

final case class Branch(instructions: List[Instruction]) extends Instruction
// defined class Branch

final case class NoOp() extends Instruction
// defined class NoOp

object Turtle {
  final case class TurtleState(at: Vec, heading: Angle)
  // defined class TurtleState

  def draw(instructions: List[Instruction]): Image = {
    def iterate(state: TurtleState, instructions: List[Instruction]): List[PathElement] =
      instructions match {
        case Nil =>
          Nil
        case i :: is =>
          val (newState, elements) = process(state, i)
          elements ++ iterate(newState, is)
      }

    def process(state: TurtleState, instruction: Instruction): (TurtleState, List[PathElement]) = {
      import PathElement._

      instruction match {
        case Forward(d) =>
          val nowAt = state.at + Vec.polar(d, state.heading)
          val element = lineTo(nowAt.toPoint)

          (state.copy(at = nowAt), List(element))
        case Turn(a) =>
          val nowHeading = state.heading + a

          (state.copy(heading = nowHeading), List())
        case Branch(is) =>
          val branchedElements = iterate(state, is)

          (state, moveTo(state.at.toPoint) :: branchedElements)
        case NoOp() =>
          (state, List())
      }
    }

    openPath(iterate(TurtleState(Vec.zero, Angle.zero), instructions))
  }
}