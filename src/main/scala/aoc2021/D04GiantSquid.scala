package aoc2021

import java.io.BufferedReader
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.jdk.StreamConverters.StreamHasToScala

private object P1 {

  import D04GiantSquid._

  def drawAndMark(b: Board, n: Int): Board = {
    b.map { rowCells =>
      rowCells.map { cell =>
        if (cell.num == n) cell.copy(mark = true)
        else cell
      }
    }
  }

  def checkForWin(b: Board): Option[Int] = {
    def checkRowOrColumn(cells: Array[Cell]): Boolean = (cells.count(_.mark) == 5)
    def compute(b: Board): Int = {
      val r: Array[Cell] = for {
        r <- b
        c <- r
        if !c.mark
      } yield (c)
      r.foldLeft(0)((acc, cell) => acc + cell.num)
    }

    for (rowCells: Array[Cell] <- b) {
      if (checkRowOrColumn(rowCells)) {
        return Some(compute(b))
      }
    }
    for (rowCells <- b.transpose) {
      if (checkRowOrColumn(rowCells)) {
        return Some(compute(b))
      }
    }
    None
  }

  def solve(b: Boards, drawnNumbers: Array[Int]): Int = {
    var boards = b
    for (n <- drawnNumbers) {
      println(n)
      boards = boards.map(ele => drawAndMark(ele, n))
      for (board <- boards) {
      checkForWin(board) match {
          case Some(v) => return v*n
          case _ =>
        }
      }
    }
    -1
  }

}

object D04GiantSquid extends App {
  case class Cell(num: Int, mark: Boolean = false)
  type Board = Array[Array[Cell]]
  type Boards = List[Board]
  type Coor = (Cell, Cell)

  @tailrec
  def buildBoards(boards: Boards, inputLines: Iterator[String]): Boards = {
    if (fIterator.hasNext) {
      fIterator.next()
      val board = fIterator.take(5)
        .toArray.map(_.strip()
        .split("\\W+")
        .map(x => Cell(x.toInt, mark = false)))
      buildBoards(boards :+ board, fIterator)
    } else {
      boards
    }
  }

  def emptyBoard = Array.ofDim[Int](5, 5)

  val filePath = "src/main/resources/D04Input.txt"
  val f: BufferedReader = Files.newBufferedReader(Paths.get(filePath), Charset.forName("UTF-8"))
  val fIterator: Iterator[String] = f.lines().toScala(Iterator)
  val drawnNumbers: Array[Int] = fIterator.next().split(",").map(_.toInt)
  val boards: Boards = (buildBoards(List[Board](), fIterator))

  assert(65325 == P1.solve(boards, drawnNumbers))
}
