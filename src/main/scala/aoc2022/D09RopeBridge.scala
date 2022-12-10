package aoc2022

import utils.AocFileOps

import scala.collection.mutable
import scala.util.{Failure, Success}

object D09RopeBridge extends App {

  def parse(input: List[String]) = input.flatMap{
      case s"R $step" => 1.to(step.toInt).foldLeft(List[(Int, Int)]()) { case (acc, _) => acc :+ (1, 0) }
      case s"D $step" => 1.to(step.toInt).foldLeft(List[(Int, Int)]()) { case (acc, _) => acc :+ (0, -1) }
      case s"L $step" => 1.to(step.toInt).foldLeft(List[(Int, Int)]()) { case (acc, _) => acc :+ (-1, 0) }
      case s"U $step" => 1.to(step.toInt).foldLeft(List[(Int, Int)]()) { case (acc, _) => acc :+ (0, 1) }
    }

  trait Coor {
    def x: Int
    def y: Int
    def move(xDis: Int, yDis: Int): Coor

  }
  case class HCoor(x: Int, y: Int) extends Coor {
    def move(xDis: Int, yDis: Int): HCoor = HCoor(x + xDis, y + yDis)
  }
  case class TCoor(x: Int, y: Int) extends Coor {
    def move(xDis: Int, yDis: Int): TCoor = TCoor(x + xDis, y + yDis)
  }

  def solve(input: List[String], ropeLength: Int): Int = {
    val l = parse(input)

    val foldedRope: Vector[Coor] =
      Vector(HCoor(0, 0)) ++   // Head knot
        Vector.fill(ropeLength-1)(TCoor(0, 0)) // Rest of the knots

    val (_, visited) = l.foldLeft(foldedRope, Set[Coor]()) { case ((rope, visited), headDisplacement) =>
      val hLocation = rope.head.move(headDisplacement._1, headDisplacement._2)

      val movedRope = rope.tail.scanLeft(hLocation) { (prevKnotLoc: Coor, thisKnotLoc: Coor) =>
        val tLocation = {
          val (hCurrX, hCurrY, tCurrX, tCurrY) = (prevKnotLoc.x, prevKnotLoc.y, thisKnotLoc.x, thisKnotLoc.y)

          (hCurrX - tCurrX, hCurrY - tCurrY) match {
            case (x, y) if x == 0 && y == 0 => TCoor(tCurrX, tCurrY)
            case (x, y) if Math.abs(x) <= 1 && Math.abs(y) <= 1 => TCoor(tCurrX, tCurrY)
            case (x, y) =>
              val applyX = if (x == 0) 0 else if (x == -2) -1 else if (x == 2) 1 else x
              val applyY = if (y == 0) 0 else if (y == -2) -1 else if (y == 2) 1 else y
              TCoor(tCurrX + applyX, tCurrY + applyY)
          }
        }
        tLocation
      }
      (movedRope, visited + rope.last)
    }
    visited.size
  }

  def solvePart1(input: List[String]) = solve(input, 2)
  def solvePart2(input: List[String]) = solve(input, 10)

  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D09TestInput.lst") match {
    case Success(input) => assert(13 == solvePart1(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D09Input.lst") match {
    case Success(input) =>
      assert(5960 == solvePart1(input))
      assert(2327 == solvePart2(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
