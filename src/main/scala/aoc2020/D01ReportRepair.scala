package aoc2020

import utils.AocFileOps

import scala.annotation.tailrec
import scala.util.{Failure, Success}

/**
 * This solution avoids creating expensive N subsets. The two sum operation, has
 * O(N) complexity. So the aggregate complexity of the first part is O(N + NLogN) => O(NLogN). The second part has
 * complexity (NLogN + N * N) => O(N^2)
 */

object D01ReportRepair extends App {

  @tailrec
  def solveTwoSum(sortedInput: List[Int], target: Int, left: Int, right: Int): Long = {
    if (left > right) return -1

    val lrSum = sortedInput(left) + sortedInput(right)
    if (lrSum == target) {
      sortedInput(left) * sortedInput(right)
    } else if (lrSum < target) {
      solveTwoSum(sortedInput, target, left+1, right)
    } else {
      solveTwoSum(sortedInput, target, left, right-1)
    }
  }

  def solvePart1(input: List[Int]): Long = solveTwoSum(input.sorted, 2020, 0, input.length-1)

  def solvePart2(input: List[Int]): Long = {
    val sortedInput = input.sorted
    sortedInput
      .indices
      .map { eleIdx => sortedInput(eleIdx) * solveTwoSum(sortedInput, 2020-sortedInput(eleIdx), eleIdx+1, sortedInput.length-1) }
      .filter(_ > 0).head
  }

  // Test cases for part 1 and 2
  assert(514579 == solvePart1(List(1721, 979, 366, 299, 675, 1456)))
  assert(241861950 == solvePart2(List(1721, 979, 366, 299, 675, 1456)))

  // Solve for part 1 and 2
  AocFileOps
    .readIntInputFromFile("src/main/resources/aoc2020/2020D01Input.lst") match {
    case Success(input) =>
      assert(290784 == solvePart1(input))
      assert(177337980 == solvePart2(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
