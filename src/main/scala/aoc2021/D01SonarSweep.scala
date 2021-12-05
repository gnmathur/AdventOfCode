package aoc2021

import aoc2021.utils.FileOperations

import scala.util.{Failure, Success}

object D01SonarSweep extends App {
  // Problem solution
  def solve(report: List[Int]): Int = {
    report.foldLeft((0, report.head)) {
      case ((acc, previous), n) =>  if (n > previous) (acc+1, n) else (acc, n)
    }._1
  }

  // -- Test Case --
  FileOperations
    .readIntVectorFromFile("src/main/resources/D01P1TestInput.txt") match {
    case Success(testVector) => assert(7 == solve(testVector))
    case Failure(exception) => println("error parsing test input")
    }

  // -- Problem Input --
  FileOperations
    .readIntVectorFromFile("src/main/resources/D01P1Input.txt") match {
    case Success(testVector) => assert(1233 == solve(testVector))
    case Failure(exception) => println("error parsing test input")
  }
}

