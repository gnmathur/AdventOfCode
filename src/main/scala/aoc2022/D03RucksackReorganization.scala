package aoc2022

import utils.AocFileOps

import scala.util.{Failure, Success}

object D03RucksackReorganization extends App {

  def charValue(ch: Char): Int = ch match {
    case _ if ch >= 'a' && ch <= 'z' => ch - 'a' + 1
    case _ => ch - 'A' + 27
  }

  def solvePart2(input: List[String]): Long = {
    val rucksacksInGroupsOf3 = input.grouped(3).toList

    rucksacksInGroupsOf3.foldLeft(0L) { (acc, groupOf3Rucksacks) =>
      val unqItemsInEach = groupOf3Rucksacks.map(_.toCharArray.toSet)
      val badgeItemType = unqItemsInEach.foldLeft(unqItemsInEach.head) { (acc, s) => acc.intersect(s) }
      acc + charValue(badgeItemType.head)
    }
  }

  def solvePart1(input: List[String]): Long = {
    input.foldLeft(0L) { (acc, rucksack) =>
      val rucksackArr = rucksack.toCharArray
      val (leftCompartment, rightCompartment) = rucksackArr.splitAt(rucksackArr.length / 2)
      val leftCSet = leftCompartment.toSet
      val rightCSet = rightCompartment.toSet
      val commonBetweenCompartments = leftCSet.intersect(rightCSet)
      acc + charValue(commonBetweenCompartments.head)
    }
  }

  /// Test case
  val input = List(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "nCrZsJsPPZsGzwwsLwLmpwMDw")
  assert(157 == solvePart1(input))
  assert(70 == solvePart2(input))

  /// Solve Part 1 and 2
  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D03Input.lst") match {
    case Success(testVector) =>
      assert(7878 == solvePart1(testVector))
      assert(2760 == solvePart2(testVector))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
