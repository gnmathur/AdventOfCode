package aoc2021

import aoc2021.utils.FileOperations

import scala.util.{Failure, Success}

object D01SonarSweepSlidingWindow extends App {
  def solve(measurements: List[Int]): Int = {
    val sumOfFirstWindow = measurements.sliding(3).next().sum
    measurements.sliding(3).foldLeft((0, sumOfFirstWindow)) {
      case ((acc, previousWindowSum), window) => {
        val thisWindowSum = window.sum
        if (thisWindowSum > previousWindowSum) (acc + 1, thisWindowSum)
        else (acc, thisWindowSum)
      }
    }._1
  }

  //
  FileOperations
    .readIntVectorFromFile("src/main/resources/D01P2TestInput.txt") match {
    case Success(testVector) => assert(5 == solve(testVector))
    case Failure(exception) => println("error parsing test input")
  }

  // -- Problem Input --
  FileOperations
    .readIntVectorFromFile("src/main/resources/D01P2Input.txt") match {
    case Success(testVector) => assert(1275 == solve(testVector))
    case Failure(exception) => println("error parsing test input")
  }
}
