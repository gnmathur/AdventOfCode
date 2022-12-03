package aoc2022

import utils.AocFileOps

import scala.collection.mutable
import scala.util.{Failure, Success}

object D03RucksackReorganization extends App {

  def charValue(ch: Char) = ch match {
    case _ if ch >= 'a' && ch <= 'z' => ch - 'a' + 1
    case _ => ch - 'A' + 27
  }

  def solvePart2(input: List[String]): Long = {
    val l: List[List[String]] = input.grouped(3).toList
    l.foldLeft(0L) { (acc, group) =>
      val groups: List[Set[Char]] = group.map(s => s.toCharArray.toSet)
      val ans: Seq[Char] = groups.foldLeft(groups(0)) { (acc, s) => acc.intersect(s) }.toSeq
      acc + charValue(ans(0))
    }
  }

  def solvePart1(input: List[String]): Long = {
    input.foldLeft(0L) { (acc, contents) =>
      val c = contents.toCharArray
      val (leftC, rightC) = c.splitAt(c.length / 2)
      val leftCSet = leftC.toSet
      val rightCSet = rightC.toSet
      val common = leftCSet.intersect(rightCSet).toSeq
      acc + charValue(common.head)
    }
  }

  val input = List(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "nCrZsJsPPZsGzwwsLwLmpwMDw")
  assert(157 == solvePart1(input))
  assert(70 == solvePart2(input))

  AocFileOps
    .readStringInputFromFile("src/main/resources/aoc2022/2022D03P1Input.lst") match {
    case Success(testVector) =>
      assert(7878 == solvePart1(testVector))
      assert(2760 == solvePart2(testVector))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
