package aoc2022

import utils.AocFileOps

import scala.util.{Failure, Success}

object D06TuningTrouble extends App {
  def solve(input: String, nDistinctChars: Int) = {
    val quadGroupWithCharIndices = input
      .toCharArray
      .zip(1 to(input.length))
      .sliding(nDistinctChars)
      .filter { arr: Array[(Char, Int)] =>
        arr.map(_._1).toSet.size == nDistinctChars
      }.toList
    quadGroupWithCharIndices.head(nDistinctChars-1)._2
  }

  // Part 1 test cases
  assert(5 == solve("bvwbjplbgvbhsrlpgdmjqwftvncz", 4))
  assert(6 == solve("nppdvjthqldpwncqszvftbrmjlhg", 4))
  assert(10 == solve("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4))
  assert(11 == solve("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4))

  // Part 2 test cases
  assert(19 == solve("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14))
  assert(23 == solve("bvwbjplbgvbhsrlpgdmjqwftvncz", 14))
  assert(23 == solve("nppdvjthqldpwncqszvftbrmjlhg", 14))
  assert(29 == solve("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14))
  assert(26 == solve("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14))

  // Solve Part 1 and 2
  AocFileOps
    .readInputAsString("src/main/resources/aoc2022/2022D06Input.lst") match {
    case Success(input) =>
      assert(1912 == solve(input, 4))
      assert(2122 == solve(input, 14))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
