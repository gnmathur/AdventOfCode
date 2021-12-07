package aoc2021

import java.io.BufferedReader
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.StreamHasToScala

object D05HydrothermalVenture extends App {
  case class Coor(col: Int, row: Int)
  case class Line(a: Coor, b: Coor)
  type Vents = Array[Array[Int]]

  def getPointsOnLine(input: Line): List[Coor] = {
    if (input.a.col == input.b.col) {
      val start = if (input.a.row < input.b.row) input.a.row else input.b.row
      val end = if (input.a.row > input.b.row) input.a.row else input.b.row
      (start to end).map(y => Coor(input.a.col, y)).toList
    } else if (input.a.row == input.b.row) {
      val start = if (input.a.col < input.b.col) input.a.col else input.b.col
      val end = if (input.a.col > input.b.col) input.a.col else input.b.col
      (start to end).map(x => Coor(x, input.a.row)).toList
    } else {
      List.empty
    }
  }

  private def markVents(g: Vents, input: List[Line]): Unit = {
    input.foreach {line =>
      val points = getPointsOnLine(line)
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

  private def findOverlapCount(g: Array[Array[Int]]): Int = {
    g.foldLeft(0) { (acc, row: Array[Int]) =>
      row.foldLeft(acc) { (acc, ele) => if (ele >= 2) acc + 1 else acc }
    }
  }

  def run(filePath: String): Int = {
    val f: BufferedReader = Files.newBufferedReader(Paths.get(filePath), Charset.forName("UTF-8"))
    val lines: List[String] = f.lines().toScala(LazyList).toList
    val parsed: List[Line] = lines map parseLine
    val (maxRow, maxCol) = findMaxRowCol(parsed)
    val g = Array.ofDim[Int](maxCol+1, maxRow+1)
    markVents(g, parsed)
    findOverlapCount(g)
  }

  assert(5 == run("src/main/resources/D05TestInput.txt"))
  assert(8060 == run("src/main/resources/D05Input.txt"))
}
