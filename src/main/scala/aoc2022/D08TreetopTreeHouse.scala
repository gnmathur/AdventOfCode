package aoc2022

import utils.AocFileOps

import scala.util.{Failure, Success}

/**
 * Notes
 * This algorithm has O(N^2) time complexity, which is not great. Also, the implementation is rather verbose.
 * A DP algorithm should be possible here
 */
object D08TreetopTreeHouse extends App {
  type Grid = List[List[Int]]
  def parse(input: List[String]): Grid = {
    input.foldLeft(List[List[Int]]()) { (acc, line) =>
      acc :+ line.toCharArray.map(_.toInt).toList
    }
  }

  def isVisible(r: Int, c: Int, R: Int, C: Int, grid: Grid): Boolean = {
    val tree = grid(r)(c)
    val result =
      (0 until c).count(index => grid(r)(index) >= tree) == 0 ||
      (c+1 until C).count(index => grid(r)(index) >= tree) == 0 ||
      (0 until r).count(index => grid(index)(c) >= tree) == 0 ||
      (r + 1 until R).count(index => grid(index)(c) >= tree) == 0
    result
  }

  def scenicCounts(r: Int, c: Int, R: Int, C: Int, grid: Grid): Long = {
    val tVal: Int = grid(r)(c)

    val left = (c-1 to 0 by -1).foldLeft((0, false)) { (acc: (Int, Boolean), idx) =>
      val diff = tVal - grid(r)(idx)
      if (!acc._2) {
        if (tVal - grid(r)(idx) > 0) (acc._1+1, false) else (acc._1+1, true)
      } else (acc)
    }._1
    val right = (c + 1 until R).foldLeft((0, false)) { (acc: (Int, Boolean), idx) =>
      val diff = tVal - grid(r)(idx)
      if (!acc._2) {
        if (tVal - grid(r)(idx) > 0) (acc._1 + 1, false) else (acc._1 + 1, true)
      } else (acc)
    }._1
    val top = (r - 1 to 0 by -1).foldLeft((0, false)) { (acc: (Int, Boolean), idx) =>
      val diff = tVal - grid(idx)(c)
      if (!acc._2) {
        if (tVal - grid(idx)(c) > 0) (acc._1 + 1, false) else (acc._1 + 1, true)
      } else (acc)
    }._1
    val bottom = (r + 1 until R).foldLeft((0, false)) { (acc: (Int, Boolean), idx) =>
      val diff = tVal - grid(idx)(c)
      if (!acc._2) {
        if (tVal - grid(idx)(c) > 0) (acc._1 + 1, false) else (acc._1 + 1, true)
      } else (acc)
    }._1
    val result = top * bottom * left * right
    result
  }

  def solvePart1(input: List[String]): Long = {
    val grid = parse(input)
    val R = grid.size
    val C = grid.head.size

    val r: Seq[Boolean] = for {
      r <- 0 until R
      c <- 0 until C
    } yield isVisible(r, c, R, C, grid)
    r.count(_ == true)
  }

  def solvePart2(input: List[String]): Long = {
    val grid = parse(input)
    val R = grid.size
    val C = grid.head.size

    val r: Seq[Long] = for {
      r <- 0 until R
      c <- 0 until C
    } yield scenicCounts(r, c, R, C, grid)
    r.max
  }

  val input = List(
    "30373",
    "25512",
    "65332",
    "33549",
    "35390"
  )
  assert(21 == solvePart1(input))
  assert(8 == solvePart2(input))

  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D08Input.lst") match {
    case Success(input) =>
      assert(1681 == solvePart1(input))
      assert(201684 == solvePart2(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }
}
