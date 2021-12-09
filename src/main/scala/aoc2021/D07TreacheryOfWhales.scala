package aoc2021

import aoc2021.utils.FileOperations

import scala.util.{Failure, Success}

object D07TreacheryOfWhales extends App {
  def fuelCostToTargetP1(target: Int, positions: List[Int]): Int =
    positions.map { p => (p-target).abs }.sum

  def fuelCostToTargetP2(target: Int, positions: List[Int]): Int =
    positions.map { p => (1 to (p-target).abs).sum }.sum

  def solve(input: List[Int], costFn: (Int, List[Int]) => Int): Int = {
    val max = input.max
    val min = input.min

    (min to max).map { p => costFn(p, input) }.min
  }

  // Test cases for part 1 and 2
  FileOperations
    .readIntFromCsvFile("src/main/resources/D07TestInput.txt") match {
    case Success(testVector) =>
      assert(37 == solve(testVector, fuelCostToTargetP1))
      assert(168 == solve(testVector, fuelCostToTargetP2))
    case Failure(exception) => println("error parsing test input")
  }

  // Problem solutions for part 1 and 2
  FileOperations
    .readIntFromCsvFile("src/main/resources/D07Input.txt") match {
    case Success(testVector) =>
      assert(341558 == solve(testVector, fuelCostToTargetP1))
      assert(93214037 == solve(testVector, fuelCostToTargetP2))
    case Failure(exception) => println("error parsing test input")
  }
}
