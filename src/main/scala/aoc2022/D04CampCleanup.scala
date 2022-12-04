package aoc2022

import utils.AocFileOps

import scala.util.{Failure, Success}

object D04CampCleanup extends App {
  def solvePart1(input: List[String]): Long = {
    input.foldLeft(0L) { (acc, line) =>
      val parts = line.split(",")
      val range1: Array[Int] = parts(0).split("-").map(_.toInt)
      val range2: Array[Int] = parts(1).split("-").map(_.toInt)

      if (range1(0) >= range2(0) && range1(1) <= range2(1)) {
        acc + 1
      } else if (range2(0) >= range1(0) && range2(1) <= range1(1)) {
        acc + 1
      } else {
        acc
      }
    }
  }

  /// Test input
  val testInput = List(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  )
  assert(2 == solvePart1(testInput))

  /// Solve Part 1 and 2
  AocFileOps
    .readStringInputFromFile("src/main/resources/aoc2022/2022D04Input.lst") match {
    case Success(testVector) =>
      assert(515 == solvePart1(testVector))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
