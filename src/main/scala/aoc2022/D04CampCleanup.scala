package aoc2022

import utils.AocFileOps

import scala.util.{Failure, Success}

object D04CampCleanup extends App {
  type SectionPair = Array[Int]
  type SectionPairs = (Array[Int], Array[Int])

  private def doParts(line: String): SectionPairs = {
    val parts = line.split(",")
    val leftPair: SectionPair = parts(0).split("-").map(_.toInt)
    val rightPair: SectionPair = parts(1).split("-").map(_.toInt)
    (leftPair, rightPair)
  }

  def solvePart2(input: List[String]): Long = {
    input.foldLeft(0L) { (acc, line) =>
      val (leftPair, rightPair) = doParts(line)

      if (leftPair(0) <= rightPair(0) && leftPair(1) >= rightPair(0))  { acc + 1 }
      else if (rightPair(0) <= leftPair(0) && rightPair(1) >= leftPair(0)) { acc + 1 }
      else { acc }
    }
  }

  def solvePart1(input: List[String]): Long = {
    input.foldLeft(0L) { (acc, line) =>
      val (leftPair, rightPair) = doParts(line)

      if (leftPair(0) >= rightPair(0) && leftPair(1) <= rightPair(1)) { acc + 1 }
      else if (rightPair(0) >= leftPair(0) && rightPair(1) <= leftPair(1)) { acc + 1 }
      else { acc }
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
  assert(4 == solvePart2(testInput))

  /// Solve Part 1 and 2
  AocFileOps
    .readStringInputFromFile("src/main/resources/aoc2022/2022D04Input.lst") match {
    case Success(testVector) =>
      assert(515 == solvePart1(testVector))
      assert(883 == solvePart2(testVector))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
