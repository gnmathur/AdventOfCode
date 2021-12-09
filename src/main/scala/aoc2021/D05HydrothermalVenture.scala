package aoc2021

import java.io.BufferedReader
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.StreamHasToScala

private object D5P1 {
  import D05HydrothermalVenture._

  def solve(g: Array[Array[Int]], parsed: List[Line]): Int = {
    D05HydrothermalVenture.markVents(g, parsed, getPointsOnLine)
    D05HydrothermalVenture.findOverlapCount(g)
  }
}

private object D5P2 {
  import D05HydrothermalVenture._

  def solve(g: Array[Array[Int]], parsed: List[Line]): Int = {
    D05HydrothermalVenture.markVents(g, parsed, getPointsOnLineEnh)
    D05HydrothermalVenture.findOverlapCount(g)
  }
}


object D05HydrothermalVenture extends App {
  case class Coor(col: Int, row: Int)
  case class Line(a: Coor, b: Coor)
  type Vents = Array[Array[Int]]

  def getPointsOnLineEnh(input: Line): List[Coor] = {
    val x1 = input.a.col
    val y1 = input.a.row
    val x2 = input.b.col
    val y2 = input.b.row

    if ((x1 == x2) || (y1 == y2)) {
      getPointsOnLine(input)
    } else {
      (
        (x1 to(x2, if (x1 < x2) 1 else -1)) zip
          (y1 to(y2, if (y1 < y2) 1 else -1)) map { ele =>
          Coor(ele._1, ele._2)
        }
      ).toList
    }
  }

  def getPointsOnLine(input: Line): List[Coor] = {
    val x1 = input.a.col
    val y1 = input.a.row
    val x2 = input.b.col
    val y2 = input.b.row

    if (x1 == x2) {
      val start = if (y1 < y2) y1 else y2
      val end = if (y1 > y2) y1 else y2
      (start to end).map(y => Coor(x1, y)).toList
    } else if (y1 == y2) {
      val start = if (x1 < x2) x1 else x2
      val end = if (x1 > x2) x1 else x2
      (start to end).map(x => Coor(x, y1)).toList
    } else {
      List.empty
    }
  }

  def markVents(g: Vents, input: List[Line], pointsFn: Line => List[Coor]): Unit = {
    input.foreach {line =>
      val points = pointsFn(line)
      points.foreach { point: Coor =>
        g(point.row)(point.col) = g(point.row)(point.col) + 1
      }
    }
  }

  private def findMaxRowCol(input: List[Line]) = {
    input.foldLeft((0, 0)) { case (acc, line) =>
      (
        Math.max(acc._1, Math.max(line.a.col, line.b.col)),
        Math.max(acc._2, Math.max(line.a.row, line.b.row))
      )
    }
  }

  private def parseLine(line: String) = {
    val coors = line.split(" -> ")
    val beginCoor = coors(0).split(",")
    val endCoor = coors(1).split(",")
    val intCoors = (beginCoor ++ endCoor) map Integer.parseInt
    Line(Coor(intCoors(0), intCoors(1)), Coor(intCoors(2), intCoors(3)))
  }

  def findOverlapCount(g: Array[Array[Int]]): Int = {
    g.flatten.foldLeft(0) { (acc, ele) => if (ele >= 2) acc + 1 else acc }
  }

  // solution runner
  def run(filePath: String, solve: (Array[Array[Int]], List[Line]) => Int): Int = {
    val f: BufferedReader = Files.newBufferedReader(Paths.get(filePath), Charset.forName("UTF-8"))
    val lines: List[String] = f.lines().toScala(LazyList).toList
    val parsed: List[Line] = lines map parseLine
    val (maxRow, maxCol) = findMaxRowCol(parsed)
    val g = Array.ofDim[Int](maxCol+1, maxRow+1)
    solve(g, parsed)
  }

  // Part 1 test and problem solution
  assert(5 == run("src/main/resources/D05TestInput.txt", D5P1.solve))
  assert(8060 == run("src/main/resources/D05Input.txt", D5P1.solve))

  // Part 2 test and problem solution
  assert(12 == run("src/main/resources/D05TestInput.txt", D5P2.solve))
  assert(21577 == run("src/main/resources/D05Input.txt", D5P2.solve))

}
