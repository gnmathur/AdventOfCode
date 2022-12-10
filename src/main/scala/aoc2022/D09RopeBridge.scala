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

  trait Location {
    def x: Int
    def y: Int
  }
  case class HLocation(x: Int, y: Int) extends Location {
    def move(xDis: Int, yDis: Int) = HLocation(x + xDis, y + yDis)
  }
  case class TLocation(x: Int, y: Int) extends Location

  def solveDay1(input: List[String]): Int = {
    val l = parse(input)
    val visited = mutable.Set[TLocation]()

    val rope = Vector(HLocation(0, 0)) ++ (1 to 10).foldLeft(Vector[Location]()){ (acc, _ ) => acc :+ TLocation(0, 0) }
    val finalLocation = l.foldLeft((HLocation(0, 0), TLocation(0, 0))) { (locations, displacement) =>
      val hLocation = locations._1.move(displacement._1, displacement._2)
      val tLocation = {
        val hCurrX = hLocation.x
        val hCurrY = hLocation.y
        val tCurrX = locations._2.x
        val tCurrY = locations._2.y

        (hCurrX - tCurrX, hCurrY - tCurrY) match {
          case (x, y) if x == 0 && y == 0 => TLocation(tCurrX, tCurrY)
          case (x, y) if Math.abs(x) <= 1 && Math.abs(y) <= 1 => TLocation(tCurrX, tCurrY)
          case (x, y) =>
            val applyX = if (x == 0) 0 else if (x == -2 ) -1 else if (x == 2) 1 else x
            val applyY = if (y == 0) 0 else if (y == -2 ) -1 else if (y == 2) 1 else y
            TLocation(tCurrX + applyX, tCurrY + applyY)
          }
        }
      visited.add(tLocation)
      (hLocation, tLocation)
      }
    visited.size
  }

  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D09TestInput.lst") match {
    case Success(input) => assert(13 == solveDay1(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D09Input.lst") match {
    case Success(input) => assert(5960 == solveDay1(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
