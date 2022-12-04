package aoc2022

import utils.AocFileOps

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

object D01CalorieCounting extends App {
  type ElfCalories = mutable.Set[Long]

  private def getCaloriesPerElf(input: List[String]): ElfCalories = {
    val elfCalories = mutable.Set[Long]()
    var index = 0
    val ab = ArrayBuffer.empty[Long]

    for (line <- input) {
      if (0 == line.compareTo("")) {
        elfCalories += ab.sum
        index += 1
        ab.clear()
      } else {
        ab.addOne(line.toLong)
      }
    }
    elfCalories
  }

  private def solvePart1(input: List[String]): Long = getCaloriesPerElf(input).max

  private def solvePart2(input: List[String]): Long = {
    implicit val ordering: Ordering[Long] = Ordering.Long.reverse

    val allElfCalories: ElfCalories = getCaloriesPerElf(input)
    val sortedElfCalories = allElfCalories.toSeq.sorted.toList
    sortedElfCalories.take(3).sum
  }

  // Test Case
  AocFileOps
    .readStringInputFromFile("src/main/resources/aoc2022/2022D01TestInput.lst") match {
    case Success(testVector) => assert(24000 == solvePart1(testVector))
    case Failure(exception) => println("error parsing test input")
  }

  // Problem - Part 1
  AocFileOps
    .readStringInputFromFile("src/main/resources/aoc2022/2022D01Input.lst") match {
    case Success(testVector) => assert(70613 == solvePart1(testVector))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

  // Problem - Part 1
  AocFileOps
    .readStringInputFromFile("src/main/resources/aoc2022/2022D01Input.lst") match {
    case Success(testVector) => assert(205805 == solvePart2(testVector))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
